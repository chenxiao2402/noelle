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
  auto heapAllocInsts = funcSum->mallocInsts;
  heapAllocInsts.insert(funcSum->callocInsts.begin(),
                        funcSum->callocInsts.end());

  /*
   * allocaCandidates is a map:
   * key: a @malloc or @calloc inst that may be transformed to allocaInst,
   *      such a inst will be called "alloca candidate".
   * value: all pointers that can direct or recursively point to the
   *        memory object allocated by the key, and their pointing levels.
   *
   * Assume we have:
   * %1 = tail call i8* @malloc(i64 8)
   * %2 = bitcast i8* %1 to i32**
   * store %2, i32*** %3
   * %4 = load i32*** %3
   *
   * We start from the memory object allocated by @malloc,
   * it will be called M1.
   *
   * The pointing level of M1 is always 0,
   * even though the data in M1 may have i32* type.
   *
   * Therefore, %1, the direct pointer to the has pointing level 1.
   * %2 is a bitcast of %1, they have the same pointing level 1,
   * even though %1 and %2 have different types.
   *
   * %2 is stored in %3, so %3 has pointing level 2 (one higher than %2).
   * %4 is loaded from %3, so %4 has pointing level 1 (one lower than %3).
   */
  unordered_map<CallBase *, unordered_map<Value *, uint64_t>> allocaCandidates;

  for (auto heapAllocInst : heapAllocInsts) {
    if (!isFixedSizedHeapAllocation(heapAllocInst)) {
      continue;
    }
    if (cfgAnalysis.isIncludedInACycle(*heapAllocInst)) {
      continue;
    }

    unordered_map<Value *, uint64_t> reachablePointers;
    if (!funcSum->isAllocableCandidate(heapAllocInst, reachablePointers)) {
      continue;
    }

    allocaCandidates[heapAllocInst] = reachablePointers;
  }

  /*
   * Compute all pointers that can direct or recursively point to
   * all memory objects allocated alloca candidates.
   */
  auto getKnownPointers = [&]() -> unordered_set<Value *> {
    unordered_set<Value *> knownPointers;
    for (auto &[_, reachablePointers] : allocaCandidates) {
      for (auto &[pointer, _] : reachablePointers) {
        knownPointers.insert(pointer);
      }
    }
    return knownPointers;
  };

  /*
   * Find direct pointers to all memory objects that may be freed by @free()
   * inst. Each pointer can either be an alloca candidate or a NULL. Here NULL
   * means the memory object is not allocated by any alloca candidate.
   */
  auto getFreedMemoryObjects =
      [&](CallBase *freeInst) -> unordered_set<CallBase *> {
    assert(funcSum->freeInsts.count(freeInst) > 0);
    auto knownPointers = getKnownPointers();
    auto freePointer = freeInst->getArgOperand(0);
    unordered_set<CallBase *> mayBeFreed;

    for (auto &[heapAllocInst, reachablePointers] : allocaCandidates) {
      if (reachablePointers.count(freePointer) == 0
          || reachablePointers[freePointer] != 1) {
        continue;
      }

      mayBeFreed.insert(heapAllocInst);
      for (auto storeInst : funcSum->storeInsts) {
        auto storePointer = storeInst->getPointerOperand();
        auto storeValue = storeInst->getValueOperand();
        if (reachablePointers.count(storePointer) > 0
            && reachablePointers[storePointer] >= 2) {
          if (knownPointers.count(storeValue) == 0) {
            mayBeFreed.insert(NULL);
          }
        }
      }
    }

    if (mayBeFreed.empty()) {
      mayBeFreed.insert(NULL);
    }

    return mayBeFreed;
  };

  /*
   * Assume we have:
   *   %1 = tail call i8* @malloc(i64 8)
   *   %2 = tail call i8* @malloc(i64 8)
   *   %3 = tail call i8* @malloc(i64 8)
   *   call free(%4);
   *   call free(%5);
   * where %1, %2 are alloca candidates; while %3 is not.
   * %4 may free memory object allocated by %1, %2.
   * %5 may free memory object allocated by %2, %3.
   *
   * It turns out we can't optimize anything.
   * Since %3 is not an alloca candidate, we can't remove @free(%5).
   * This means %2 should not be transformed to allocaInst either.
   * Since %2 can't be transformed to allocaInst, we can't remove @free(%4).
   * This means %1 also shouldn't be transformed to allocaInst.
   *
   * Therefore, if the memory object allocated by an alloca candidate
   * can be freed by a @free() inst that may also free memory objecys
   * allocated by a non-alloca-candidate, the alloca candidate is not
   * an alloca candidate anymore.
   */
  bool fixedPoint = false;
  while (!fixedPoint) {
    fixedPoint = true;
    for (auto freeInst : funcSum->freeInsts) {
      auto mayBeFreed = getFreedMemoryObjects(freeInst);
      if (mayBeFreed.count(NULL) > 0) {
        for (auto heapAllocInst : mayBeFreed) {
          if (heapAllocInst != NULL
              && allocaCandidates.count(heapAllocInst) > 0) {
            allocaCandidates.erase(heapAllocInst);
            fixedPoint = false;
          }
        }
      }
    }
  }

  /*
   * The remaining alloca candidates can be safely transformed to allocaInst.
   */
  unordered_set<CallBase *> allocable;
  for (auto &[heapAllocInst, _] : allocaCandidates) {
    allocable.insert(heapAllocInst);
  }

  /*
   * If a @free() inst can only free memory objects allocated by the remaining
   * alloca candidates, we can safely remove the @free() inst.
   */
  unordered_set<CallBase *> removable;
  for (auto freeInst : funcSum->freeInsts) {
    auto mayBeFreed = getFreedMemoryObjects(freeInst);
    if (mayBeFreed.count(NULL) == 0) {
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
