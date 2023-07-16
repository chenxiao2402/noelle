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

/*
 * Compute @malloc() or @calloc() insts that could be transformed to allocaInst.
 * Compute @free() insts that could be removed becase if @malloc() is
 * transformed to allocaInst, the corresponding @free() must be removed.
 */
LiveMemorySummary PrivatizerManager::getLiveMemorySummary(Noelle &noelle,
                                                          Function *f) {

  auto cfgAnalysis = noelle.getCFGAnalysis();
  auto funcSum = functionSummaries[f];

  /*
   * Only fixed size @malloc(), such as %1 = tail call i8* @malloc(i64 8), can
   * be transformed to allocaInst. Otherwise, it may cause stack overflow.
   */
  unordered_set<CallBase *> allocable;
  auto heapAllocInsts = funcSum->mallocInsts;
  heapAllocInsts.insert(funcSum->callocInsts.begin(),
                        funcSum->callocInsts.end());

  for (auto heapAllocInst : heapAllocInsts) {
    if (!isFixedSizedHeapAllocation(heapAllocInst)) {
      continue;
    }
    if (cfgAnalysis.isIncludedInACycle(*heapAllocInst)) {
      continue;
    }
    if (!funcSum->isAllocableCandidate(heapAllocInst)) {
      continue;
    }
    allocable.insert(heapAllocInst);
  }

  if (allocable.empty()) {
    return LiveMemorySummary();
  }

  unordered_set<CallBase *> removable = funcSum->freeInsts;
  for (auto freeInst : funcSum->freeInsts) {
    auto ptr = dyn_cast<CallBase>(freeInst->getArgOperand(0));
    if (!ptr || allocable.count(ptr) == 0) {
      allocable.clear();
      removable.clear();
      break;
    } else {
      removable.insert(freeInst);
    }
  }

  LiveMemorySummary memSum = LiveMemorySummary();
  memSum.removable = removable;
  memSum.allocable = allocable;

  return memSum;
}

unordered_map<Function *, LiveMemorySummary> PrivatizerManager::
    collectHeapToStack(Noelle &noelle) {

  unordered_map<Function *, LiveMemorySummary> result;

  for (auto &[f, funcSum] : functionSummaries) {
    auto fname = f->getName();
    auto suffix = " in function " + fname + "\n";

    if (funcSum->mallocInsts.empty() && funcSum->callocInsts.empty()) {
      errs() << prefix << "@malloc or @calloc not invoked" << suffix;
      continue;
    }

    auto memSum = getLiveMemorySummary(noelle, f);
    if (memSum.allocable.empty()) {
      errs() << prefix << "@malloc or @calloc not allocable" << suffix;
      continue;
    }

    result[f] = memSum;
  }

  return result;
}

bool PrivatizerManager::applyHeapToStack(Noelle &noelle,
                                         LiveMemorySummary liveMemSum) {
  bool modified = false;

  for (auto heapAllocInst : liveMemSum.allocable) {
    auto allocationSize = getAllocationSize(heapAllocInst);
    auto currentF = heapAllocInst->getParent()->getParent();
    auto funcSum = functionSummaries[currentF];
    auto suffix = " in function " + currentF->getName() + "\n";

    if (!funcSum->stackHasEnoughSpaceForNewAllocaInst(allocationSize)) {
      errs()
          << prefix
          << "Stack memory usage exceeds the limit, can't transfrom to allocaInst: "
          << *heapAllocInst << suffix;
      continue;
    }

    errs()
        << prefix << "Transform @malloc() or @calloc() to allocaInst" << suffix;

    modified = true;
    auto entryBlock = &currentF->getEntryBlock();
    auto firstInst = entryBlock->getFirstNonPHI();
    IRBuilder<> entryBuilder(firstInst);
    IRBuilder<> allocBuilder(heapAllocInst);

    LLVMContext &context = noelle.getProgramContext();
    Type *oneByteType = Type::getInt8Ty(context);
    ConstantInt *arraySize =
        ConstantInt::get(Type::getInt64Ty(context), allocationSize);

    auto calleeFunc = heapAllocInst->getCalledFunction();
    auto calleeName = calleeFunc ? calleeFunc->getName() : "";

    if (calleeName == "malloc") {
      AllocaInst *allocaInst =
          entryBuilder.CreateAlloca(oneByteType, arraySize, "");

      errs() << prefix << "Replace @malloc: " << *heapAllocInst << "\n";
      errs() << emptyPrefix << "with allocaInst: " << *allocaInst << "\n";

      heapAllocInst->replaceAllUsesWith(allocaInst);
      heapAllocInst->eraseFromParent();

    } else if (calleeName == "calloc") {
      ConstantInt *zeroVal = ConstantInt::get(Type::getInt8Ty(context), 0);

      AllocaInst *allocaInst =
          entryBuilder.CreateAlloca(oneByteType, arraySize, "");

      CallInst *memSetInst =
          allocBuilder.CreateMemSet(allocaInst, zeroVal, arraySize, 1);

      errs() << prefix << "Replace @malloc: " << *heapAllocInst << "\n";
      errs() << emptyPrefix << "with allocaInst: " << *allocaInst << "\n";
      errs() << emptyPrefix << "and memsetInst: " << *memSetInst << "\n";

      heapAllocInst->replaceAllUsesWith(allocaInst);
      heapAllocInst->eraseFromParent();
    }
  }
  for (auto freeInst : liveMemSum.removable) {
    freeInst->eraseFromParent();
  }
  return modified;
}

} // namespace llvm::noelle
