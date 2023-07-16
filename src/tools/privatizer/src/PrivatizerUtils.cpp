/*
 * Copyright 2023 Xiao Chen
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 OR OTHER DEALINGS IN THE SOFTWARE.
 */
#include "PrivatizerManager.hpp"

namespace llvm::noelle {

enum MPAFunctionType {
  MALLOC,
  CALLOC,
  REALLOC,
  FREE,
  INTRINSIC,
  READ_ONLY,
  MEM_COPY,
  USER_DEFINED,
  UNKNOWN
};

MPAFunctionType getCalleeFunctionType(CallBase *callInst) {
  const set<std::string> READ_ONLY_LIB_FUNCTIONS = {
    "atoi",   "atof",    "atol",   "atoll",  "fprintf", "fputc", "fputs",
    "putc",   "putchar", "printf", "puts",   "rand",    "scanf", "sqrt",
    "strlen", "strncmp", "strtod", "strtol", "strtoll"
  };

  const set<std::string> READ_ONLY_LIB_FUNCTIONS_WITH_SUFFIX =
      [&]() -> std::set<std::string> {
    std::set<std::string> result;
    for (auto fname : READ_ONLY_LIB_FUNCTIONS) {
      result.insert(fname);
      result.insert(fname + "_unlocked");
    }
    return result;
  }();

  auto isLifetimeIntrinsic = [](CallBase *callInst) {
    auto intrinsic = dyn_cast<IntrinsicInst>(callInst);
    if (!intrinsic) {
      return false;
    }
    return intrinsic->isLifetimeStartOrEnd();
  };

  auto calleeFunc = callInst->getCalledFunction();
  auto fname = calleeFunc ? calleeFunc->getName() : "";

  if (fname == "malloc") {
    return MALLOC;
  } else if (fname == "calloc") {
    return CALLOC;
  } else if (fname == "realloc") {
    return REALLOC;
  } else if (fname == "free") {
    return FREE;
  } else if (isLifetimeIntrinsic(callInst)) {
    return INTRINSIC;
  } else if (READ_ONLY_LIB_FUNCTIONS_WITH_SUFFIX.count(fname) > 0) {
    return READ_ONLY;
  } else if (isa<MemCpyInst>(callInst)) {
    return MEM_COPY;
  } else if ((calleeFunc != nullptr) && (!calleeFunc->isDeclaration())) {
    return USER_DEFINED;
  } else {
    return UNKNOWN;
  }
}

FunctionSummary::FunctionSummary(Function *currentF) {
  this->currentF = currentF;
  this->stackMemoryUsage = 0;
  auto dl = currentF->getParent()->getDataLayout();

  for (auto &bb : *currentF) {
    for (auto &inst : bb) {
      if (isa<StoreInst>(inst)) {
        auto storeInst = dyn_cast<StoreInst>(&inst);
        this->storeInsts.insert(storeInst);
      }
      if (isa<CallBase>(inst)) {
        auto callInst = dyn_cast<CallBase>(&inst);
        auto functionType = getCalleeFunctionType(callInst);
        switch (functionType) {
          case MALLOC:
            this->mallocInsts.insert(callInst);
            break;
          case CALLOC:
            this->callocInsts.insert(callInst);
            break;
          case REALLOC:
            this->reallocInsts.insert(callInst);
            break;
          case FREE:
            this->freeInsts.insert(callInst);
            break;
          default:
            break;
        }
      } else if (isa<AllocaInst>(inst)) {
        auto allocaInst = dyn_cast<AllocaInst>(&inst);
        stackMemoryUsage +=
            allocaInst->getAllocationSizeInBits(dl).getValue() / 8;
      }
    }
  }
}

bool FunctionSummary::stackHasEnoughSpaceForNewAllocaInst(
    uint64_t allocationSize) {
  if ((stackMemoryUsage + allocationSize) < STACK_SIZE_THRESHOLD) {
    stackMemoryUsage += allocationSize;
    return true;
  } else {
    return false;
  }
}

bool FunctionSummary::isAllocableCandidate(Value *source) {
  auto heapAllocOrGlobalVar = false;
  if (isa<GlobalVariable>(source)) {
    heapAllocOrGlobalVar = true;
  } else if (isa<CallBase>(source)) {
    auto callInst = dyn_cast<CallBase>(source);
    auto calleeType = getCalleeFunctionType(callInst);
    if (calleeType == MALLOC || calleeType == CALLOC) {
      heapAllocOrGlobalVar = true;
    }
  }

  if (!heapAllocOrGlobalVar) {
    return false;
  }

  unordered_set<Value *> reachableValues;
  queue<pair<Value *, int>> worklist;
  worklist.push(make_pair(source, 1));

  while (!worklist.empty()) {
    auto [v, ptLevel] = worklist.front();
    worklist.pop();
    if (reachableValues.find(v) != reachableValues.end()) {
      continue;
    }
    reachableValues.insert(v);

    for (auto user : v->users()) {
      if (isa<BitCastInst>(user) || isa<BitCastOperator>(user)) {
        worklist.push(make_pair(user, ptLevel));
      } else if (isa<GetElementPtrInst>(user) || isa<GEPOperator>(user)) {
        worklist.push(make_pair(user, ptLevel));
      } else if (isa<PHINode>(user) || isa<SelectInst>(user)) {
        worklist.push(make_pair(user, ptLevel));
      } else if (isa<LoadInst>(user)) {
        if (ptLevel > 1) {
          worklist.push(make_pair(user, ptLevel - 1));
        }
      } else if (isa<StoreInst>(user)) {
        auto storeInst = dyn_cast<StoreInst>(user);
        if (storeInst->getValueOperand() == v) {
          worklist.push(make_pair(storeInst->getPointerOperand(), ptLevel + 1));
        }
      } else if (isa<CallBase>(user)) {
        auto callInst = dyn_cast<CallBase>(user);
        auto calleeFuncType = getCalleeFunctionType(callInst);

        if (calleeFuncType == MALLOC || calleeFuncType == CALLOC
            || calleeFuncType == REALLOC || calleeFuncType == FREE
            || calleeFuncType == INTRINSIC || calleeFuncType == READ_ONLY) {
          continue;
        } else if (calleeFuncType == MEM_COPY) {
          auto memcpyInst = dyn_cast<MemCpyInst>(callInst);
          if (memcpyInst->getDest() == v) {
            return false;
          } else if (memcpyInst->getSource() == v) {
            worklist.push(make_pair(memcpyInst->getDest(), ptLevel));
          }
        } else if (calleeFuncType == USER_DEFINED
                   || calleeFuncType == UNKNOWN) {
          auto calleeFunc = callInst->getCalledFunction();
          if (calleeFunc == nullptr) {
            return false;
          }
          for (auto &arg : callInst->arg_operands()) {
            auto operand = callInst->getArgOperand(arg.getOperandNo());
            auto argument = calleeFunc->args().begin() + arg.getOperandNo();
            if (operand == v) {
              if (!argument->hasNoCaptureAttr()) {
                return false;
              }
            }
          }
        } else if (isa<Argument>(user) || isa<ReturnInst>(user)) {
          return false;
        } else {
          worklist.push(make_pair(user, ptLevel));
        }
      }
    }
  }

  return true;
}

bool PrivatizerManager::isFixedSizedHeapAllocation(CallBase *heapAllocInst) {
  auto calleeType = getCalleeFunctionType(heapAllocInst);
  if (calleeType == MALLOC) {
    if (dyn_cast<ConstantInt>(heapAllocInst->getOperand(0))) {
      return true;
    }
  } else if (calleeType == CALLOC) {
    if (dyn_cast<ConstantInt>(heapAllocInst->getOperand(0))
        && dyn_cast<ConstantInt>(heapAllocInst->getOperand(1))) {
      return true;
    }
  }
  return false;
}

uint64_t PrivatizerManager::getAllocationSize(Value *allocationSource) {
  if (isa<CallBase>(allocationSource)) {
    auto heapAllocInst = dyn_cast<CallBase>(allocationSource);
    if (isFixedSizedHeapAllocation(heapAllocInst)) {
      auto dl = M->getDataLayout();
      auto calleeType = getCalleeFunctionType(heapAllocInst);
      if (calleeType == MALLOC) {
        return dyn_cast<ConstantInt>(heapAllocInst->getOperand(0))
            ->getZExtValue();
      } else if (calleeType == CALLOC) {
        auto elementCount =
            dyn_cast<ConstantInt>(heapAllocInst->getOperand(0))->getZExtValue();
        auto elementSizeInBytes =
            dyn_cast<ConstantInt>(heapAllocInst->getOperand(1))->getZExtValue();
        return elementCount * elementSizeInBytes;
      }
    }
  } else if (isa<GlobalVariable>(allocationSource)) {
    auto globalVar = dyn_cast<GlobalVariable>(allocationSource);
    auto globalVarType = globalVar->getValueType();
    auto dl = globalVar->getParent()->getDataLayout();
    return dl.getTypeAllocSize(globalVarType);
  }
  assert(
      false
      && "Unsupported allocation source: not a fixed-sized heap allocation or a global variable.");
};

unordered_set<Function *> PrivatizerManager::functionsInvokedFrom(
    Noelle &noelle,
    Function *caller) {

  auto fm = noelle.getFunctionsManager();
  auto mainF = fm->getEntryFunction();
  auto pcf = fm->getProgramCallGraph();

  auto insertMyCallees = [&](Function *caller,
                             queue<Function *> &funcToTraverse) {
    auto funcNode = pcf->getFunctionNode(caller);
    for (auto callEdge : funcNode->getOutgoingEdges()) {
      for (auto subEdge : callEdge->getSubEdges()) {
        auto calleeFunc = subEdge->getCallee()->getFunction();
        if (!calleeFunc || calleeFunc->empty()) {
          continue;
        }
        funcToTraverse.push(calleeFunc);
      }
    }
  };

  unordered_set<Function *> funcSet;
  queue<Function *> funcToTraverse;
  insertMyCallees(caller, funcToTraverse);

  while (!funcToTraverse.empty()) {
    auto func = funcToTraverse.front();
    funcToTraverse.pop();
    if (funcSet.count(func) > 0) {
      continue;
    }
    funcSet.insert(func);
    insertMyCallees(func, funcToTraverse);
  }

  return funcSet;
};

UserSummary::UserSummary(GlobalVariable *globalVar) {
  queue<User *> worklist;
  queue<bool> isDirectUser;
  unordered_map<User *, unordered_set<User *>> inst2op;
  for (auto user : globalVar->users()) {
    worklist.push(user);
    isDirectUser.push(true);
  }

  while (!worklist.empty()) {
    auto user = worklist.front();
    auto direct = isDirectUser.front();
    worklist.pop();
    isDirectUser.pop();

    if (isa<Instruction>(user)) {
      auto inst = dyn_cast<Instruction>(user);
      auto f = inst->getFunction();
      if (direct) {
        userInsts[f].insert(inst);
        users[f].insert(inst);
      } else {
        userInsts[f].insert(inst);
        for (auto op : inst2op[inst]) {
          users[f].insert(op);
        }
      }
    } else if (isa<Operator>(user)) {
      auto op = dyn_cast<Operator>(user);
      for (auto opUser : op->users()) {
        worklist.push(opUser);
        isDirectUser.push(false);
        inst2op[opUser].insert(user);
      }
    }
  }

  for (auto &[f, insts] : userInsts) {
    userFunctions.insert(f);
  }
};

} // namespace llvm::noelle
