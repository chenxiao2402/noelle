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
#include "llvm/IR/Instruction.h"

namespace llvm::noelle {

class LiveMemorySummary {
public:
  std::unordered_set<CallBase *> allocable;
  std::unordered_set<CallBase *> removable;
};

class FunctionSummary {
public:
  FunctionSummary(Function *currentF);

  Function *currentF;

  std::unordered_set<CallBase *> mallocInsts;
  std::unordered_set<CallBase *> callocInsts;
  std::unordered_set<CallBase *> reallocInsts;
  std::unordered_set<CallBase *> freeInsts;
  std::unordered_set<StoreInst *> storeInsts;

  bool isAllocableCandidate(Value *source);
  bool stackHasEnoughSpaceForNewAllocaInst(uint64_t allocationSize);

private:
  const uint64_t STACK_SIZE_THRESHOLD = 8 * 1024 * 1024;
  uint64_t stackMemoryUsage;
};

class UserSummary {
public:
  UserSummary(GlobalVariable *globalVar);

  GlobalVariable *globalVar;
  std::unordered_set<Function *> userFunctions;
  std::unordered_map<Function *, std::unordered_set<User *>> users;
  std::unordered_map<Function *, std::unordered_set<Instruction *>> userInsts;

  std::unordered_map<User *, std::unordered_set<Instruction *>> user2Insts;
};

class PrivatizerManager : public ModulePass {
public:
  static char ID;

  // PrivatizerManager.cpp
  PrivatizerManager();

  bool doInitialization(Module &M) override;

  bool runOnModule(Module &M) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override;

private:
  Module *M;

  bool enablePrivatizer;

  const string prefix = "PrivatizerManager: ";

  const string emptyPrefix = "                   ";

  unordered_map<Function *, FunctionSummary *> functionSummaries;

  // PrivatizerUtils.cpp
  bool isFixedSizedHeapAllocation(CallBase *heapAllocInst);

  uint64_t getAllocationSize(Value *allocationSource);

  unordered_set<Function *> functionsInvokedFrom(Noelle &noelle,
                                                 Function *caller);

  // HeapToStack.cpp
  unordered_map<Function *, LiveMemorySummary> collectHeapToStack(
      Noelle &noelle);

  bool applyHeapToStack(Noelle &noelle, LiveMemorySummary liveMemSum);

  LiveMemorySummary getLiveMemorySummary(Noelle &noelle, Function *f);

  // GlobalToStack.cpp
  unordered_map<GlobalVariable *, unordered_set<Function *>>
  collectGlobalToStack(Noelle &noelle);

  bool applyGlobalToStack(Noelle &noelle,
                          GlobalVariable *globalVar,
                          unordered_set<Function *> privatizableFunctions);

  unordered_set<Function *> getPrivatizableFunctions(Noelle &noelle,
                                                     GlobalVariable *globalVar);

  Instruction *getProgramPointOfInitilization(
      Noelle &noelle,
      GlobalVariable *globalVar,
      StoreInst *storeInst,
      unordered_set<Instruction *> &initializers);

  bool globalVariableInitializedInFunction(Noelle &noelle,
                                           GlobalVariable *globalVar,
                                           Function *currentF);
};

} // namespace llvm::noelle
