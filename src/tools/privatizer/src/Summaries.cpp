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

FunctionSummary::FunctionSummary(Function *currentF, bool printMpaInfo)
  : currentF(currentF),
    printMpaInfo(printMpaInfo) {

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
   * 1. Collect store/alloca/malloc/calloc/free insts in the function.
   * 2. Collect all pointers in the function.
   * 3. Collect all pointers may be returned by the function.
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
        allocaInsts.insert(allocaInst);
        insertPointer(allocaInst);
        stackMemoryUsage += getAllocationSize(allocaInst);
      } else if (isa<CallBase>(inst)) {
        auto callInst = dyn_cast<CallBase>(&inst);
        auto calleeType = getCalleeFunctionType(callInst);
        switch (calleeType) {
          case MALLOC:
            mallocInsts.insert(callInst);
            insertPointer(callInst);
            break;
          case CALLOC:
            callocInsts.insert(callInst);
            insertPointer(callInst);
            break;
          case FREE:
            freeInsts.insert(callInst);
            break;
          case REALLOC:
            insertPointer(callInst);
            break;
          default:
            insertPointer(callInst);
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
  if (!mpaFinished) {
    allocaCandidate = nullptr;
    mayPointsToAnalysis();
  }
  auto memObjId = memobj2nodeId[heapAllocInst];
  return mayBePointedByUnknown(memObjId);
}

bool FunctionSummary::notPrivatizable(GlobalVariable *globalVar) {
  allocaCandidate = globalVar;
  mayPointsToAnalysis();
  auto memObjId = memobj2nodeId[globalVar];
  return mayBePointedByUnknown(memObjId);
}

bool FunctionSummary::isDestOfMemcpy(Value *v) {
  assert(mpaFinished && (ptr2nodeId.find(v) != ptr2nodeId.end()));
  auto nodeId = ptr2nodeId[v];
  return destsOfMemcpy.find(nodeId) != destsOfMemcpy.end();
}

bool FunctionSummary::mayBePointedByUnknown(NodeID nodeId) {
  auto unknownMemobjId = 0;
  auto unknownPts = getPointees(unknownMemobjId);
  assert(0 <= nodeId && nodeId < unknownPts.size());
  if (unknownPts[nodeId]) {
    return true;
  }
  for (auto retPtr : returnPointers) {
    auto retPtrId = getPtrId(retPtr);
    auto retMemobjs = getReachableMemobjs(retPtrId);
    if (retMemobjs.find(nodeId) != retMemobjs.end()) {
      return true;
    }
  }
  return false;
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

bool FunctionSummary::stackCanHoldNewAlloca(uint64_t allocationSize) {
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
  mpaFinished = true;

  if (printMpaInfo) {
    errs() << "May Points-to Analysis for function @" << currentF->getName()
           << "\n";

    errs() << "NodeIDs of memory objects:\n";
    for (auto &[allocation, memobjId] : memobj2nodeId) {
      if (allocation) {
        errs() << "\tmemobjId : " << memobjId << " , " << *allocation << "\n";
      } else {
        errs() << "\tmemobjId : " << memobjId << " , "
               << "UNKNOWN"
               << "\n";
      }
    }

    errs() << "NodeIDs of pointers:\n";
    for (auto &[ptr, ptrId] : ptr2nodeId) {
      errs() << "\tptrId : " << ptrId << " , " << *ptr << "\n";
    }

    errs() << "Points-to relation:\n";
    for (auto &[nodeId, pointees] : pointsTo) {
      errs() << "\t" << nodeId << " -> ";
      for (auto pointee : pointees.set_bits()) {
        errs() << pointee << ", ";
      }
      errs() << "\n";
    }

    errs() << "Copy out edges:\n";
    for (auto &[src, dests] : copyOutEdges) {
      errs() << "\t" << src << " => ";
      for (auto dest : dests) {
        errs() << dest << ", ";
      }
      errs() << "\n";
    }

    errs() << "Incoming stores:\n";
    for (auto &[ptrId, stores] : incomingStores) {
      for (auto store : stores) {
        errs() << "\t" << ptrId << " => " << *store << "\n";
      }
    }

    errs() << "Outgoing loads:\n";
    for (auto &[ptrId, loads] : outgoingLoads) {
      for (auto load : loads) {
        errs() << "\t" << ptrId << " => " << *load << "\n";
      }
    }
  }
}

void FunctionSummary::initPtInfo(void) {

  mpaFinished = false;
  nextNodeId = 0;
  ptr2nodeId.clear();
  memobj2nodeId.clear();
  nodeId2memobj.clear();
  pointsTo.clear();
  copyOutEdges.clear();
  incomingStores.clear();
  outgoingLoads.clear();
  usedAsFuncArg.clear();
  destsOfMemcpy.clear();

  auto allocations = getAllocations();

  /*
   * Assign node ids to memory objects
   * 1. Unknown memobj refers to all memobjs allocated in other functions
   *    and memobjs of global variables. It's nodeId is always 0.
   * 2. For each memobj allocated by alloca/malloc/calloc, assign a unique
   *    nodeId.
   * 3. If a global variable is allocaCandidate, its memobj is not unknown,
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
   * 1. Assign nodeId to each pointer.
   * NOTE: The nodeId for the pointer is different from the nodeId for the
   * memobj. For example, if we have %1 = alloca i32, the nodeId for the
   * allocated memobj may be 1, the nodeId for the pointer %1 may be 4.
   *
   * 2. Initialize points-to information
   * The unknown memobj conservatively points to itself since it's a summary.
   * Pointers of alloca/malloc/calloc point to the allocated memobj. Arguments
   * of current function, global variables and returned pointers of callInsts
   * will point to unknown memobj (allocaCandidate is the only exception).
   *
   * 3. Add copy edges for pointers.
   * Copy edges between pointers can be added through PHINode, SelectInst,
   * MemcpyInst and realloc(). "unknown" memobj can be have copy edges from
   * arguments of callInsts, and copy edges to returned pointers of callInsts.
   *
   * 4. Record uses of pointers.
   * If a pointer is used as the pointer operand of a store/load instruction,
   * or it is used as an argument of a callInst, record these uses since they
   * can help the may points-to analysis add more copy edges.
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
      auto calleeType = getCalleeFunctionType(callInst);
      switch (calleeType) {
        case REALLOC:
          addCopyEdge(getPtrId(callInst->getArgOperand(0)), ptrId);
          break;
        case USER_DEFINED:
        case UNKNOWN:
          pointsTo[ptrId] = onlyPointsTo(unknownMemobjId);
          addCopyEdge(unknownMemobjId, ptrId);
          break;
        default:
          break;
      }
    } else if (isa<ConstantPointerNull>(ptr)) {
      pointsTo[ptrId] = getEmptyBitVector();
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
        auto calleeType = getCalleeFunctionType(callInst);
        switch (calleeType) {
          case MEM_COPY:
            destsOfMemcpy.insert(getPtrId(callInst->getArgOperand(0)));
            addCopyEdge(getPtrId(callInst->getArgOperand(1)),
                        getPtrId(callInst->getArgOperand(0)));
            break;
          case USER_DEFINED:
          case UNKNOWN:
            usedAsFuncArg.insert(ptrId);
            addCopyEdge(ptrId, unknownMemobjId);
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
    /*
     * OutgoingLoads help us add new copy edges.
     *
     * For example, if pointers (%p2) and memobjs (@M1, @M2, @M3, @M4)
     * form this points-to graph:
     *
     * %p2 -> @M2, @M3; @M2 -> @M1; @M3 -> @M4,
     *
     * then `%3 = load i32*, i32** %p2` can add copy edges
     * "@M2 => %3" and "@M3 => %3"
     *
     * %3 will point to @M1 and @M4.
     */
    if (outgoingLoads.find(ptrId) != outgoingLoads.end()) {
      for (auto loadInst : outgoingLoads[ptrId]) {
        auto destId = getPtrId(loadInst);
        if (addCopyEdge(memobjId, destId)) {
          worklist.push(memobjId);
        }
      }
    }

    /*
     * IncomingStores help us add new copy edges.
     *
     * For example, if pointers (%p1, %p2) and memobjs (@M1, @M2, @M3)
     * form this points-to graph:
     *
     * %p1 -> @M1 ; %p2 -> @M2, @M3
     *
     * Then `store i32* %p1, i32** %p2` add copy edges "%p1 => @M2" and
     * "%p1 => @M3", and the points-to graph can be updated:
     *
     * ... ; @M2 -> @M1; @M3 -> @M1
     */
    if (incomingStores.find(ptrId) != incomingStores.end()) {
      for (auto storeInst : incomingStores[ptrId]) {
        auto srcId = getPtrId(storeInst->getValueOperand());
        if (addCopyEdge(srcId, memobjId)) {
          worklist.push(srcId);
        }
      }
    }
  }
}

void FunctionSummary::handleFuncUsers(NodeID ptrId) {
  if (usedAsFuncArg.find(ptrId) == usedAsFuncArg.end()) {
    return;
  }
  /*
   * If a pointer is used as argument of a callInst, then all memobjds
   * reachable from this pointer escape. To preserve conservativeness,
   * "unknown" memobj and all escaped memobj point to each other.
   *
   * 1. Add copy edge between "unknown" memobj and all escaped memobjds.
   * If memobj @M2 escaped and its points-to info updated:
   *    pts(@M2) = ... U { @M1 },
   * such an update should be propagated because @M1 escaped too:
   *    pts("unknown") = ... U { @M1 }.
   *
   * 2. Add copy edge from the pointer argument to "unknown" memobj.
   * Assume pointer %p1 is used as argument of a callInst `call @g(%p1)`,
   * and we have this points-to graph:
   *    %p1 -> @M1, @M2 ; @M1 -> @M3, @M4
   * Adding copy edges between "unknown" memobj and { @M1, @M2, @M3, @M4 }
   * is not enough because @M1 and @M2 are not pointed by any escaped memobj.
   * Hence "unknown" memobj will not point to @M1 and @M2. To solve this issue,
   * we need also to add copy edges from  %p1 to "unknown" memobj so that
   * "unkonwn" can point to @M1 and @M2.
   *
   * 3. Add copy edge from "unknown" memobj to the return value.
   * If the return value of the callInst is a pointer, it will conservatively
   * point to "unknown" memobj and all escaped memobj.
   *
   * Here we only handle case 1. Case 2 and 3 are already handled by
   * initPtInfo().
   */
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
  if (copyOutEdges.find(srcId) == copyOutEdges.end()) {
    return;
  }
  /*
   * Propogate the points-to info from srcId to destId through copy edges.
   * i.e. pts(destId) = pts(destId) U pts(srcId).
   * If pts(destId) is changed, add destId to worklist.
   */
  for (auto &destId : copyOutEdges[srcId]) {
    if (unionPts(srcId, destId)) {
      worklist.push(destId);
    }
  }
}

bool FunctionSummary::unionPts(NodeID srcId, NodeID dstId) {
  BitVector oldPts = pointsTo[dstId];
  BitVector newPts = unite(oldPts, pointsTo[srcId]);
  if (oldPts == newPts) {
    return false;
  } else {
    pointsTo[dstId] = newPts;
    return true;
  }
}

UserSummary::UserSummary(GlobalVariable *globalVar, Noelle &noelle) {
  auto hotFuncs = hotFunctions(noelle);
  queue<User *> worklist;
  queue<bool> isDirectUser;
  unordered_map<Instruction *, unordered_set<User *>> inst2op;
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
      if (hotFuncs.find(f) != hotFuncs.end()) {
        if (direct) {
          userInsts[f].insert(inst);
          users[f].insert(inst);
        } else {
          userInsts[f].insert(inst);
          for (auto op : inst2op[inst]) {
            users[f].insert(op);
          }
        }
      }
    } else if (isa<Operator>(user)) {
      auto op = dyn_cast<Operator>(user);
      for (auto opUser : op->users()) {
        if (isa<Instruction>(opUser)) {
          auto inst = dyn_cast<Instruction>(opUser);
          worklist.push(inst);
          isDirectUser.push(false);
          inst2op[inst].insert(op);
        }
      }
    }
  }

  for (auto &[f, insts] : userInsts) {
    userFunctions.insert(f);
  }
};

} // namespace llvm::noelle
