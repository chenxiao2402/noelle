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

MPAFunctionType getCalleeFunctionType(CallBase *callInst);

bool isFixedSizedHeapAllocation(CallBase *heapAllocInst);

/*
 * Get the size of the allocated memory object in bytes.
 */
uint64_t getAllocationSize(Value *allocationSource);

/*
 * Collected all functions that are called directly or indirectly by caller.
 * Caller itself will not be included unless it's called recursively.
 */
unordered_set<Function *> functionsInvokedFrom(Noelle &noelle,
                                               Function *caller);
/*
 * All functions reachable from @main.
 */
unordered_set<Function *> hotFunctions(Noelle &noelle);

/*
 * Strip pointer casts and GEPs.
 */
Value *strip(Value *pointer);

BitVector unite(const BitVector &lhs, const BitVector &rhs);

bool isSubsetOf(const unordered_set<CallBase *> &subset,
                const unordered_set<CallBase *> &superset);

} // namespace llvm::noelle
