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
#include "Summaries.hpp"

namespace llvm::noelle {

class Privatizer : public ModulePass {
public:
  static char ID;

  // Privatizer.cpp
  Privatizer();

  bool doInitialization(Module &M) override;

  bool runOnModule(Module &M) override;

  void getAnalysisUsage(AnalysisUsage &AU) const override;

private:
  Module *M;

  bool enablePrivatizer;

  const string prefix = "Privatizer: ";

  const string emptyPrefix = "            ";

  unordered_map<Function *, FunctionSummary *> functionSummaries;

  bool verbose;

  FunctionSummary *getFunctionSummary(Function *f);

  void clearFunctionSummaries();

  // HeapToStack.cpp
  bool applyH2S(Noelle &noelle);

  unordered_map<Function *, LiveMemorySummary> collectH2S(Noelle &noelle);

  bool transformH2S(Noelle &noelle, LiveMemorySummary liveMemSum);

  LiveMemorySummary getLiveMemorySummary(Noelle &noelle, Function *f);

  // GlobalToStack.cpp
  bool applyG2S(Noelle &noelle);

  unordered_map<GlobalVariable *, unordered_set<Function *>> collectG2S(
      Noelle &noelle);

  bool transformG2S(Noelle &noelle,
                    GlobalVariable *globalVar,
                    unordered_set<Function *> privatizable);

  unordered_set<Function *> getPrivatizableFunctions(Noelle &noelle,
                                                     GlobalVariable *globalVar);

  Instruction *getInitProgramPoint(Noelle &noelle,
                                   GlobalVariable *globalVar,
                                   StoreInst *storeInst,
                                   unordered_set<Instruction *> &initializers);

  bool initializedBeforeAllUse(Noelle &noelle,
                               GlobalVariable *globalVar,
                               Function *currentF);
};

} // namespace llvm::noelle
