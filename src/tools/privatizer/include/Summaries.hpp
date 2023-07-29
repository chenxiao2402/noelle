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
  FunctionSummary(Function *currentF, bool printMpaInfo);

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

  bool insertNewAllocaInst(uint64_t allocationSize);

private:
  bool printMpaInfo = false;
  const uint64_t STACK_SIZE_THRESHOLD = 8 * 1024 * 1024;
  uint64_t stackMemoryUsage = 0;

  bool mpaFinished = false;
  NodeID nextNodeId = 0;

  /*
   * All pointers may be used as return value of current function.
   */
  unordered_set<Value *> returnPointers;
  /*
   * All pointers in current function.
   */
  unordered_set<Value *> pointers;

  /*
   * Assign node id to each pointer in current function.
   */
  unordered_map<Value *, NodeID> ptr2nodeId;

  /*
   * Assign node id to each memory object, the memory object is represented
   * by the souce of allocation.
   *
   * 1) The allocation source can be a AllocaInst, a malloc/calloc call,
   *    or allocaCandidate.
   * 2) Besides, we have a "unknown" memobj, which always has node id 0 and
   *    its allocation souce is nullptr, which is a summary of all memobjs not
   *    allocated by current function.
   *
   * Arguments of current function and global variables point to "unknown"
   * memobj. Pointers returned by callInsts point to "unknown" memobj. "unkonwn"
   * memobj points to itself since it's a summary.
   */
  unordered_map<Value *, NodeID> memobj2nodeId;
  unordered_map<NodeID, Value *> nodeId2memobj;

  /*
   * The memory objects pointed by pointers and memory objects.
   */
  unordered_map<NodeID, BitVector> pointsTo;

  /*
   * A copy edge (src => dest) means that dest may point to the same memobjs
   * as src. To be more specific, pts(dest) = pts(dest) U pts(src).
   * Here both src and dest can be pointers or memobjs.
   *
   * For example. if we have %1 = select i1 %cond, i32* %ptr1, i32* %ptr2,
   * then we add two copy edges: "%ptr1 => %1" and "%ptr2 => %1",
   * and we have pts(%1) = pts(%ptr1) U pts(%ptr2).
   */
  unordered_map<NodeID, unordered_set<NodeID>> copyOutEdges;

  /*
   * storeInst = `store i32* %p1, i32** %p2` is an incomingStore of %p2
   * since %p2 is used as the pointer operand of storeInst, and the points-to
   * info flows from %p1 to the memory objects pointed by %p2.
   */
  unordered_map<NodeID, unordered_set<StoreInst *>> incomingStores;

  /*
   * `%3 = load i32*, i32** %p2` is an outgoingLoad of %p2 since %p2 is used
   * as the pointer operand of loadInst, the points-to info flows out of the
   * memobjs pointed by %2 to the loadInst.
   */
  unordered_map<NodeID, unordered_set<LoadInst *>> outgoingLoads;

  /*
   * All pointers used as arguments of call sites in current function.
   */
  unordered_set<NodeID> usedAsFuncArg;

  /*
   * Because of the issue of -instCombine pass, an allocaInst will be
   * incorrectly removed if it's the dest of memcpy calls. So we track all dests
   * of memcpy calls and they should not be transformed to allocaInsts.
   */
  unordered_set<NodeID> destOfMemcpy;

  /*
   * allocaCandidate is a global variable that we want to privatize into
   * current function, i.e. it works like an allocaInst in current function.
   *
   * Usually, global variables point to "unknown" memobj (see comments below),
   * but allocaCandidate is an exception. We assign a node id for the memobj
   * of allocaCandidate, which is different from all other memobjs from
   * alloca/malloc/calloc and "unkonwn" memobj.
   *
   * Check GlobalToStack.cpp for the reason to introduce allocaCandidate.
   */
  GlobalVariable *allocaCandidate = nullptr;

  queue<NodeID> worklist;

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
  UserSummary(GlobalVariable *globalVar, Noelle &noelle);

  GlobalVariable *globalVar;
  /*
   * All functions that use globalVar.
   */
  unordered_set<Function *> userFunctions;
  /*
   * All users of globalVar classified by function,
   * a user can be an Instruction or an Operator.
   */
  unordered_map<Function *, unordered_set<User *>> users;
  /*
   * All instructions that use globalVar in each function.
   * Instructions may use globalVar directly or indirectly.
   *
   * Direct: `%16 = load i64*, i64** @array, align 8`.
   * Indirect: `store i8* %16, i8** bitcast (i64** @array to i8**), align 8`.
   *
   * The two instructions use global variable @array, so they are both
   * in the `userInsts` set of @array.
   *
   * However, only the loadInst uses @array directly, the storeInst only uses
   * the BitCastOpearator of @array. As a result, the `users` set of @array will
   * contain the loadInst and `i8** bitcast (i64** @array to i8**)`.
   */
  unordered_map<Function *, unordered_set<Instruction *>> userInsts;
};

} // namespace llvm::noelle
