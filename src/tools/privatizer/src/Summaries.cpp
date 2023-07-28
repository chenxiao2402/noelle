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
#include "Privatizer.hpp"
#include "Summaries.hpp"
#include "Utils.hpp"

namespace llvm::noelle {

FunctionSummary::FunctionSummary(Function *currentF) {
  this->currentF = currentF;
  this->stackMemoryUsage = 0;
  auto dl = currentF->getParent()->getDataLayout();

  auto insertPointer = [&](Value *v) {
    if (v->getType()->isPointerTy()) {
      pointers.insert(v);
      auto stripped = strip(v);
      if (stripped != v) {
        pointers.insert(stripped);
      }
    }
  };

  /*
   * (1) Collect alloca/malloc/calloc/free insts in the function.
   * (2) Collect all pointers in the function.
   * (3) Collect all pointers may be returned by the function.
   */

  for (auto &arg : currentF->args()) {
    insertPointer(&arg);
  }

  for (auto &bb : *currentF) {
    for (auto &inst : bb) {
      if (isa<LoadInst>(inst)) {
        auto loadInst = dyn_cast<LoadInst>(&inst);
        insertPointer(loadInst->getPointerOperand());
        insertPointer(loadInst);
      } else if (isa<StoreInst>(inst)) {
        auto storeInst = dyn_cast<StoreInst>(&inst);
        storeInsts.insert(storeInst);
        insertPointer(storeInst->getValueOperand());
        insertPointer(storeInst->getPointerOperand());
      } else if (isa<BitCastInst>(inst) || isa<GetElementPtrInst>(inst)) {
        insertPointer(&inst);
      } else if (isa<AllocaInst>(inst)) {
        auto allocaInst = dyn_cast<AllocaInst>(&inst);
        insertPointer(allocaInst);
        this->allocaInsts.insert(allocaInst);
        stackMemoryUsage +=
            allocaInst->getAllocationSizeInBits(dl).getValue() / 8;
      } else if (isa<CallBase>(inst)) {
        auto callInst = dyn_cast<CallBase>(&inst);
        auto functionType = getCalleeFunctionType(callInst);
        switch (functionType) {
          case MALLOC:
            insertPointer(callInst);
            this->mallocInsts.insert(callInst);
            break;
          case CALLOC:
            insertPointer(callInst);
            this->callocInsts.insert(callInst);
            break;
          case FREE:
            this->freeInsts.insert(callInst);
            break;
          case REALLOC:
            insertPointer(callInst);
            break;
          default:
            if (callInst->getType()->isPointerTy()) {
              insertPointer(callInst);
            }
            for (auto &arg : callInst->arg_operands()) {
              auto operand = callInst->getArgOperand(arg.getOperandNo());
              insertPointer(operand);
            }
            break;
        }
      } else if (isa<ReturnInst>(inst)) {
        auto returnInst = dyn_cast<ReturnInst>(&inst);
        auto retVal = returnInst->getReturnValue();
        if (retVal && retVal->getType()->isPointerTy()) {
          returnPointers.insert(retVal);
        }
      }
    }
  }
}

bool FunctionSummary::mayEscape(CallBase *heapAllocInst) {
  assert(mallocInsts.find(heapAllocInst) != mallocInsts.end()
         || callocInsts.find(heapAllocInst) != callocInsts.end());
  if (nextNodeId == 0) {
    mayPointsToAnalysis();
  }
  auto memObjId = memobj2nodeId[heapAllocInst];
  return escaped(memObjId);
}

bool FunctionSummary::mayEscape(GlobalVariable *globalVar) {
  allocaCandidate = globalVar;
  mayPointsToAnalysis();
  auto memObjId = memobj2nodeId[globalVar];
  return escaped(memObjId);
}

bool FunctionSummary::isDestOfMemcpy(Value *v) {
  assert(nextNodeId > 0);
  if (ptr2nodeId.find(v) == ptr2nodeId.end()) {
    return false;
  }
  auto nodeId = ptr2nodeId[v];
  return destOfMemcpy.find(nodeId) != destOfMemcpy.end();
}

bool FunctionSummary::escaped(NodeID nodeId) {
  auto unknownMemobjId = 0;
  auto escaped = getPointees(unknownMemobjId);
  assert(0 <= nodeId && nodeId < escaped.size());
  return escaped[nodeId];
}

unordered_set<CallBase *> FunctionSummary::getFreedMemobjs(CallBase *freeInst) {
  assert(freeInsts.find(freeInst) != freeInsts.end());
  auto freePtr = freeInst->getArgOperand(0);
  auto freePtrId = getPtrId(freePtr);
  auto mayBeFreed = getPointees(freePtrId);
  unordered_set<CallBase *> freedMemObjs;
  for (auto memobjId : mayBeFreed.set_bits()) {
    auto heapAllocInst = nodeId2memobj[memobjId];
    if (!heapAllocInst) {
      freedMemObjs.insert(nullptr);
    } else if (heapAllocInst && isa<CallBase>(heapAllocInst)) {
      freedMemObjs.insert(dyn_cast<CallBase>(heapAllocInst));
    }
  }
  return freedMemObjs;
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

BitVector FunctionSummary::getPointees(NodeID nodeId) {
  if (pointsTo.find(nodeId) != pointsTo.end()) {
    return pointsTo[nodeId];
  } else {
    return getEmptyBitVector();
  }
}

unordered_set<NodeID> FunctionSummary::getReachableMemobjs(NodeID ptrId) {
  unordered_set<NodeID> reachable;
  queue<NodeID> todolist;
  todolist.push(ptrId);
  while (!todolist.empty()) {
    auto nodeId = todolist.front();
    todolist.pop();

    for (auto memObj : getPointees(nodeId).set_bits()) {
      if (reachable.find(memObj) == reachable.end()) {
        reachable.insert(memObj);
        todolist.push(memObj);
      }
    }
  }
  return reachable;
}

BitVector FunctionSummary::getEmptyBitVector(void) {
  auto bitVecSize = 1 + getAllocations().size();
  return BitVector(bitVecSize, false);
}

BitVector FunctionSummary::onlyPointsTo(NodeID memobjId) {
  auto emptyPts = getEmptyBitVector();
  assert(memobjId >= 0 && memobjId < emptyPts.size());
  emptyPts.set(memobjId);
  return emptyPts;
}

unordered_set<Value *> FunctionSummary::getAllocations(void) {
  unordered_set<Value *> allocations;
  for (auto &allocaInst : allocaInsts) {
    allocations.insert(allocaInst);
  }
  for (auto &mallocInst : mallocInsts) {
    allocations.insert(mallocInst);
  }
  for (auto &callocInst : callocInsts) {
    allocations.insert(callocInst);
  }
  if (allocaCandidate) {
    allocations.insert(allocaCandidate);
  }
  return allocations;
}

NodeID FunctionSummary::getPtrId(Value *v) {
  assert(v->getType()->isPointerTy());
  auto stripped = strip(v);
  if (ptr2nodeId.find(stripped) == ptr2nodeId.end()) {
    ptr2nodeId[stripped] = nextNodeId++;
  }
  return ptr2nodeId[stripped];
}

bool FunctionSummary::addCopyEdge(NodeID src, NodeID dst) {
  return copyOutEdges[src].insert(dst).second;
}

void FunctionSummary::mayPointsToAnalysis(void) {
  initPtInfo();
  solveWorklist();
}

void FunctionSummary::initPtInfo(void) {

  nextNodeId = 0;
  ptr2nodeId.clear();
  nodeId2memobj.clear();
  pointsTo.clear();

  auto allocations = getAllocations();

  /*
   * Assign node ids to memory objects
   * 1. Unknown memobj refers to all memobjs allocated in other functions
   *    and memobjs of global variables. It's nodeId is always 0.
   * 2. For each memobj allocated by alloca/malloc/calloc, assign a unique
   *    nodeId.
   * 3. If a global variable is a alloca candidate, its memobj is not unknown,
   *    it will be assigned a unique nodeId.
   */
  auto unknownMemobjId = nextNodeId++;
  nodeId2memobj[unknownMemobjId] = nullptr;
  memobj2nodeId[nullptr] = unknownMemobjId;

  for (auto &ptr : allocations) {
    auto nodeId = nextNodeId++;
    nodeId2memobj[nodeId] = ptr;
    memobj2nodeId[ptr] = nodeId;
  }

  /*
   * (1) Assign nodeId to each pointer.
   *     Note the nodeId for the pointer is different from the nodeId for the
   * memobj. For example, if we have %1 = alloca i32, the nodeId for the
   * allocated memobj may be 1, the nodeId for the pointer %1 may be 4.
   *
   * (2) Initialize points-to information
   *     1. The unknown memobj conservatively points to itself since it's a
   * summary.
   *     2. Pointers of alloca/malloc/calloc point to the allocated memobj.
   *     3. Arguments of current function and results of callInsts point to
   * unknown memobj.
   *
   * (3) Add copy edges for pointers.
   *     If we have %1 = select i1 %cond, i32* %ptr1, i32* %ptr2,
   *     then we add two copy edges: "%ptr1 -> %1" and "%ptr2 -> %1", where
   * %ptr1 and %ptr2 are sources of the copy edges, %1 is the destination of the
   * copy edges. A copy edge from %ptr2 to %1 means that %1 may points to the
   * same memobjs as %ptr1, to be more specific, pts(%1) = pts(%1) U pts(%ptr2)
   *
   * (4) If a pointer is used as the pointer operand of a store/load
   * instruction, or it is used as an operand of CallInst, record these uses
   * since they can help the may point-to analysis.
   */
  pointsTo[unknownMemobjId] = onlyPointsTo(unknownMemobjId);

  for (auto &ptr : pointers) {
    auto ptrId = getPtrId(ptr);

    if (allocations.find(ptr) != allocations.end()) {
      auto memobjId = memobj2nodeId[ptr];
      pointsTo[ptrId] = onlyPointsTo(memobjId);
    } else if (isa<PHINode>(ptr)) {
      auto phiNode = dyn_cast<PHINode>(ptr);
      for (auto &incoming : phiNode->incoming_values()) {
        auto incomingPtrId = getPtrId(incoming);
        addCopyEdge(incomingPtrId, ptrId);
      }
    } else if (isa<SelectInst>(ptr)) {
      auto selectInst = dyn_cast<SelectInst>(ptr);
      auto trueValuePtrId = getPtrId(selectInst->getTrueValue());
      auto falseValuePtrId = getPtrId(selectInst->getFalseValue());
      addCopyEdge(trueValuePtrId, ptrId);
      addCopyEdge(falseValuePtrId, ptrId);
    } else if (isa<Argument>(ptr) || isa<GlobalVariable>(ptr)) {
      pointsTo[ptrId] = onlyPointsTo(unknownMemobjId);
    } else if (isa<CallBase>(ptr)) {
      auto callInst = dyn_cast<CallBase>(ptr);
      auto functionType = getCalleeFunctionType(callInst);
      switch (functionType) {
        case REALLOC:
          addCopyEdge(getPtrId(callInst->getArgOperand(0)), ptrId);
          break;
        case USER_DEFINED:
        case UNKNOWN:
          pointsTo[ptrId] = onlyPointsTo(unknownMemobjId);
          addCopyEdge(unknownMemobjId, ptrId);
          for (auto &arg : callInst->arg_operands()) {
            auto operand = callInst->getArgOperand(arg.getOperandNo());
            if (operand->getType()->isPointerTy()) {
              addCopyEdge(getPtrId(operand), unknownMemobjId);
            }
          }
          break;
        default:
          break;
      }
    }

    for (auto user : ptr->users()) {
      if (isa<StoreInst>(user)) {
        auto storeInst = dyn_cast<StoreInst>(user);
        auto valueOperand = storeInst->getValueOperand();
        if (ptr == storeInst->getPointerOperand()
            && valueOperand->getType()->isPointerTy()) {
          incomingStores[ptrId].insert(storeInst);
        }
      } else if (isa<LoadInst>(user)) {
        auto loadInst = dyn_cast<LoadInst>(user);
        if (ptr == loadInst->getPointerOperand()
            && loadInst->getType()->isPointerTy()) {
          outgoingLoads[ptrId].insert(loadInst);
        }
      } else if (isa<CallBase>(user)) {
        auto callInst = dyn_cast<CallBase>(user);
        auto functionType = getCalleeFunctionType(callInst);
        switch (functionType) {
          case MEM_COPY:
            addCopyEdge(getPtrId(callInst->getArgOperand(1)),
                        getPtrId(callInst->getArgOperand(0)));
            destOfMemcpy.insert(getPtrId(callInst->getArgOperand(0)));
            break;
          case USER_DEFINED:
          case UNKNOWN:
            usedAsFuncArg.insert(ptrId);
          default:
            break;
        }
      }
    }
  }
}

void FunctionSummary::solveWorklist(void) {
  worklist = {};
  for (auto &[ptr, ptrId] : ptr2nodeId) {
    worklist.push(ptrId);
  }

  while (!worklist.empty()) {
    auto nodeID = worklist.front();
    worklist.pop();
    handleLoadStore(nodeID);
    handleFuncUsers(nodeID);
    handleCopyEdges(nodeID);
  }
}

void FunctionSummary::handleLoadStore(NodeID ptrId) {
  auto pointees = getPointees(ptrId);
  for (auto memobjId : pointees.set_bits()) {
    for (auto loadInst : outgoingLoads[ptrId]) {
      auto destId = getPtrId(loadInst);
      if (addCopyEdge(memobjId, destId)) {
        worklist.push(memobjId);
      }
    }
    for (auto storeInst : incomingStores[ptrId]) {
      auto srcId = getPtrId(storeInst->getValueOperand());
      if (addCopyEdge(srcId, memobjId)) {
        worklist.push(srcId);
      }
    }
  }
}

void FunctionSummary::handleFuncUsers(NodeID ptrId) {
  if (usedAsFuncArg.find(ptrId) == usedAsFuncArg.end()) {
    return;
  }

  NodeID unknownMemobjId = 0;
  for (auto memobjId : getReachableMemobjs(ptrId)) {
    bool changed = false;
    changed |= addCopyEdge(memobjId, unknownMemobjId);
    changed |= addCopyEdge(unknownMemobjId, memobjId);
    if (changed) {
      worklist.push(memobjId);
    }
  }
}

void FunctionSummary::handleCopyEdges(NodeID srcId) {
  auto pointees = getReachableMemobjs(srcId);
  for (auto &destId : copyOutEdges[srcId]) {
    if (unionPts(srcId, destId)) {
      worklist.push(destId);
    }
  }
}

bool FunctionSummary::unionPts(NodeID srcId, NodeID dstId) {
  BitVector oldPts = pointsTo[dstId];
  pointsTo[dstId] |= pointsTo[srcId];
  return pointsTo[dstId] != oldPts;
}

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
        inst2op[opUser].insert(op);
      }
    }
  }

  for (auto &[f, insts] : userInsts) {
    userFunctions.insert(f);
  }
};

} // namespace llvm::noelle
