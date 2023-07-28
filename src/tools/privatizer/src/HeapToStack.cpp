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
#include "Utils.hpp"

namespace llvm::noelle {

bool Privatizer::applyH2S(Noelle &noelle) {
  bool modified = false;
  for (auto &[f, liveMemSum] : collectH2S(noelle)) {
    modified |= transformH2S(noelle, liveMemSum);
  }
  clearFunctionSummaries();
  return modified;
}

/*
 * Compute @malloc() or @calloc() insts that could be transformed to allocaInst.
 * Compute @free() insts that could be removed becase if @malloc() is
 * transformed to allocaInst, the corresponding @free() must be removed.
 */
LiveMemorySummary Privatizer::getLiveMemorySummary(Noelle &noelle,
                                                   Function *f) {

  auto cfgAnalysis = noelle.getCFGAnalysis();
  auto funcSum = getFunctionSummary(f);

  /*
   * Only fixed size @malloc(), such as %1 = tail call i8* @malloc(i64 8), can
   * be transformed to allocaInst. Otherwise, it may cause stack overflow.
   */
  auto heapAllocInsts = funcSum->mallocInsts;
  heapAllocInsts.insert(funcSum->callocInsts.begin(),
                        funcSum->callocInsts.end());

  /*
   * allocable: @malloc() or @calloc() insts that could be transformed to
   * allocaInst.
   */
  unordered_set<CallBase *> allocable;

  for (auto heapAllocInst : heapAllocInsts) {
    if (!isFixedSizedHeapAllocation(heapAllocInst)) {
      continue;
    }
    if (cfgAnalysis.isIncludedInACycle(*heapAllocInst)) {
      continue;
    }
    if (funcSum->mayEscape(heapAllocInst)
        || funcSum->isDestOfMemcpy(heapAllocInst)) {
      continue;
    }

    allocable.insert(heapAllocInst);
  }

  /*
   * Assume we have:
   *   %1 = tail call i8* @malloc(i64 8)
   *   %2 = tail call i8* @malloc(i64 8)
   *   %3 = tail call i8* @malloc(i64 8)
   *   call free(%4);
   *   call free(%5);
   * where %1, %2 are allocable; while %3 is not.
   * %4 may free memory object allocated by %1, %2.
   * %5 may free memory object allocated by %2, %3.
   *
   * It turns out we can't optimize anything.
   * Since %3 is not an allocable, we can't remove @free(%5).
   * This means %2 should not be transformed to allocaInst either.
   * Since %2 can't be transformed to allocaInst, we can't remove @free(%4).
   * This means %1 also shouldn't be transformed to allocaInst.
   *
   * Therefore, if the memory object allocated by an allocable
   * can be freed by a @free() inst that may also free memory objecys
   * allocated by a non-allocable, the allocable is not
   * an allocable anymore.
   */
  bool fixedPoint = false;
  while (!fixedPoint) {
    fixedPoint = true;
    for (auto freeInst : funcSum->freeInsts) {
      auto mayBeFreed = funcSum->getFreedMemobjs(freeInst);
      if (!isSubsetOf(mayBeFreed, allocable)) {
        for (auto heapAllocInst : mayBeFreed) {
          if (allocable.find(heapAllocInst) != allocable.end()) {
            allocable.erase(heapAllocInst);
            fixedPoint = false;
          }
        }
      }
    }
  }

  /*
   * If a @free() inst can only free memory objects allocated by the remaining
   * allocable, we can safely remove the @free() inst.
   */
  unordered_set<CallBase *> removable;
  for (auto freeInst : funcSum->freeInsts) {
    auto mayBeFreed = funcSum->getFreedMemobjs(freeInst);
    if (isSubsetOf(mayBeFreed, allocable)) {
      removable.insert(freeInst);
    }
  }

  LiveMemorySummary memSum = LiveMemorySummary();
  memSum.removable = removable;
  memSum.allocable = allocable;

  return memSum;
}

unordered_map<Function *, LiveMemorySummary> Privatizer::collectH2S(
    Noelle &noelle) {

  unordered_set<Function *> heapAllocUsers;
  for (auto &F : *M) {
    if (F.getName() != "malloc" && F.getName() != "calloc") {
      continue;
    }
    for (auto user : F.users()) {
      if (auto callInst = dyn_cast<CallBase>(user)) {
        heapAllocUsers.insert(callInst->getFunction());
      }
    }
  }

  unordered_map<Function *, LiveMemorySummary> result;

  for (auto f : heapAllocUsers) {
    auto fname = f->getName();
    auto suffix = " in function " + fname + "\n";
    auto funcSum = getFunctionSummary(f);
    auto memSum = getLiveMemorySummary(noelle, f);

    if (memSum.allocable.empty()) {
      errs() << prefix << "@malloc or @calloc not allocable" << suffix;
      continue;
    }

    result[f] = memSum;
  }

  return result;
}

bool Privatizer::transformH2S(Noelle &noelle, LiveMemorySummary liveMemSum) {
  bool modified = false;

  for (auto heapAllocInst : liveMemSum.allocable) {
    auto allocationSize = getAllocationSize(heapAllocInst);
    auto currentF = heapAllocInst->getParent()->getParent();
    auto funcSum = getFunctionSummary(currentF);
    auto suffix = " in function " + currentF->getName() + "\n";

    if (!funcSum->stackHasEnoughSpaceForNewAllocaInst(allocationSize)) {
      errs()
          << prefix
          << "Stack memory usage exceeds the limit, can't transfrom to allocaInst: "
          << *heapAllocInst << suffix;
      continue;
    }

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
      errs() << emptyPrefix << "and memset Inst: " << *memSetInst << "\n";

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
