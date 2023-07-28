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
#pragma once

#include "noelle/core/Noelle.hpp"

namespace llvm::noelle {

typedef uint64_t NodeID;

class LiveMemorySummary {
public:
  unordered_set<CallBase *> allocable;
  unordered_set<CallBase *> removable;
};

class FunctionSummary {
public:
  FunctionSummary(Function *currentF);

  Function *currentF;

  unordered_set<StoreInst *> storeInsts;
  unordered_set<AllocaInst *> allocaInsts;
  unordered_set<CallBase *> mallocInsts;
  unordered_set<CallBase *> callocInsts;
  unordered_set<CallBase *> freeInsts;

  bool mayEscape(CallBase *heapAllocInst);
  bool mayEscape(GlobalVariable *globalVar);
  bool isDestOfMemcpy(Value *v);

  unordered_set<CallBase *> getFreedMemobjs(CallBase *freeInst);

  bool stackHasEnoughSpaceForNewAllocaInst(uint64_t allocationSize);

private:
  const uint64_t STACK_SIZE_THRESHOLD = 8 * 1024 * 1024;
  uint64_t stackMemoryUsage;

  NodeID nextNodeId = 0;
  GlobalVariable *allocaCandidate;
  queue<NodeID> worklist;

  unordered_set<Value *> returnPointers;
  unordered_set<Value *> pointers;

  unordered_map<Value *, NodeID> ptr2nodeId;
  unordered_map<Value *, NodeID> memobj2nodeId;
  unordered_map<NodeID, Value *> nodeId2memobj;
  unordered_map<NodeID, BitVector> pointsTo;

  unordered_map<NodeID, unordered_set<NodeID>> copyOutEdges;
  unordered_map<NodeID, unordered_set<StoreInst *>> incomingStores;
  unordered_map<NodeID, unordered_set<LoadInst *>> outgoingLoads;
  unordered_set<NodeID> usedAsFuncArg;

  unordered_set<NodeID> destOfMemcpy;

  BitVector getEmptyBitVector(void);
  BitVector onlyPointsTo(NodeID memobjId);
  unordered_set<Value *> getAllocations(void);
  NodeID getPtrId(Value *v);
  bool addCopyEdge(NodeID src, NodeID dst);

  void mayPointsToAnalysis(void);
  void initPtInfo(void);
  void solveWorklist(void);

  void handleLoadStore(NodeID ptrId);
  void handleFuncUsers(NodeID ptrId);
  void handleCopyEdges(NodeID srcId);

  BitVector getPointees(NodeID nodeId);
  unordered_set<NodeID> getReachableMemobjs(NodeID ptrId);
  bool unionPts(NodeID srcId, NodeID dstId);

  bool escaped(NodeID nodeId);
};

class UserSummary {
public:
  UserSummary(GlobalVariable *globalVar);

  GlobalVariable *globalVar;
  unordered_set<Function *> userFunctions;
  unordered_map<Function *, unordered_set<User *>> users;
  unordered_map<Function *, unordered_set<Instruction *>> userInsts;

  unordered_map<User *, unordered_set<Instruction *>> user2Insts;
};

} // namespace llvm::noelle
