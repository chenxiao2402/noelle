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
#include "noelle/core/Noelle.hpp"
#include "Privatizer.hpp"

namespace llvm::noelle {

Privatizer::Privatizer() : ModulePass{ ID } {
  return;
}

bool Privatizer::runOnModule(Module &M) {

  /*
   * Check if enablers have been enabled.
   */
  if (!this->enablePrivatizer) {
    return false;
  }
  auto prefix = "Privatizer: ";
  errs() << prefix << "Start\n";

  /*
   * Fetch NOELLE.
   */
  auto &noelle = getAnalysis<Noelle>();
  verbose = noelle.getVerbosity() > Verbosity::Maximal;

  auto modified = false;
  modified |= applyG2S(noelle);
  modified |= applyH2S(noelle);

  return modified;
}

FunctionSummary *Privatizer::getFunctionSummary(Function *f) {
  if (functionSummaries.find(f) == functionSummaries.end()) {
    functionSummaries[f] = new FunctionSummary{ f, verbose };
  }
  return functionSummaries[f];
}

void Privatizer::clearFunctionSummaries() {
  for (auto &[f, summary] : functionSummaries) {
    delete summary;
  }
  functionSummaries.clear();
}

} // namespace llvm::noelle
