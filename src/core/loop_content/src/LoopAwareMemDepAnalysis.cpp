/*
 * Copyright 2016 - 2024  Angelo Matni, Simone Campanoni, Brian Homerding
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
#include "noelle/core/DataFlow.hpp"
#include "noelle/core/LoopCarriedDependencies.hpp"
#include "noelle/core/LoopAliasAnalysisEngine.hpp"
#include "noelle/core/LoopContent.hpp"
#include "LoopAwareMemDepAnalysis.hpp"

/*
 * SCAF headers
 */
#define ENABLE_SCAF
#ifdef ENABLE_SCAF
#  include "scaf/MemoryAnalysisModules/LoopAA.h"
#  include "scaf/Utilities/PDGQueries.h"
#  include "scaf/Utilities/ModuleLoops.h"
#endif

namespace arcana::noelle {

/*
 * SCAF
 */
#ifdef ENABLE_SCAF
static liberty::LoopAA *NoelleSCAFAA = nullptr;
static liberty::ModuleLoops *ModuleLoops = nullptr;
#endif

class NoelleSCAFIntegration : public ModulePass {
public:
  static char ID;

  NoelleSCAFIntegration();
  bool doInitialization(Module &M) override;
  void getAnalysisUsage(AnalysisUsage &AU) const override;
  bool runOnModule(Module &M) override;
};

// Next there is code to register your pass to "opt"
char NoelleSCAFIntegration::ID = 0;
static RegisterPass<NoelleSCAFIntegration> X("noellescaf",
                                             "Integration with SCAF");

// Next there is code to register your pass to "clang"
static NoelleSCAFIntegration *_PassMaker = nullptr;
static RegisterStandardPasses _RegPass1(
    PassManagerBuilder::EP_OptimizerLast,
    [](const PassManagerBuilder &, legacy::PassManagerBase &PM) {
      if (!_PassMaker) {
        PM.add(_PassMaker = new NoelleSCAFIntegration());
      }
    }); // ** for -Ox
static RegisterStandardPasses _RegPass2(
    PassManagerBuilder::EP_EnabledOnOptLevel0,
    [](const PassManagerBuilder &, legacy::PassManagerBase &PM) {
      if (!_PassMaker) {
        PM.add(_PassMaker = new NoelleSCAFIntegration());
      }
    }); // ** for -O0

void refinePDGWithLoopAwareMemDepAnalysis(LDGGenerator &ldgAnalysis,
                                          PDG *loopDG,
                                          Loop *l,
                                          LoopStructure *loopStructure,
                                          LoopTree *loops,
                                          LoopIterationSpaceAnalysis *LIDS) {
  refinePDGWithSCAF(loopDG, l);

  if (LIDS) {
    refinePDGWithLIDS(loopDG, loopStructure, loops, LIDS);
  }

  /*
   * Run the loop-centric data dependence analyses.
   */
  ldgAnalysis.improveDependenceGraph(loopDG, loopStructure);

  return;
}

void refinePDGWithSCAF(PDG *loopDG, Loop *l) {
#ifdef ENABLE_SCAF
  assert(NoelleSCAFAA != nullptr);

  // replace it to the correct one for SCAF
  auto li = &ModuleLoops->getAnalysis_LoopInfo(l->getHeader()->getParent());
  l = li->getLoopFor(l->getHeader());

  /*
   * Iterate over all the edges of the loop PDG and collect memory deps to be
   * queried. For each pair of instructions with a memory dependence map it to
   * a small vector of found edges (0th element is for RAW, 1st for WAW, 2nd for
   * WAR)
   */
  std::map<std::pair<Instruction *, Instruction *>,
           SmallVector<DGEdge<Value, Value> *, 3>>
      memDeps;
  for (auto edge : make_range(loopDG->begin_edges(), loopDG->end_edges())) {

    /*
     * Skip dependences that are not between instructions of the target loop
     */
    if (!loopDG->isInternal(edge->getDst())
        || !loopDG->isInternal(edge->getSrc())) {
      continue;
    }

    /*
     * If the dependence is not via memory, then SCAF cannot help.
     */
    if (!isa<MemoryDependence<Value, Value>>(edge)) {
      continue;
    }
    auto memDep = cast<MemoryDependence<Value, Value>>(edge);

    /*
     * Fetch the instructions involved in the dependence.
     */
    auto pdgValueI = memDep->getSrc();
    auto i = dyn_cast<Instruction>(pdgValueI);
    assert(i && "Expecting an instruction as the value of a PDG node");

    auto pdgValueJ = memDep->getDst();
    auto j = dyn_cast<Instruction>(pdgValueJ);
    assert(j && "Expecting an instruction as the value of a PDG node");

    if (!memDeps.count({ i, j })) {
      memDeps[{ i, j }] = { nullptr, nullptr, nullptr };
    }

    if (memDep->isRAWDependence()) {
      memDeps[{ i, j }][0] = memDep;
    } else if (memDep->isWAWDependence()) {
      memDeps[{ i, j }][1] = memDep;
    } else if (memDep->isWARDependence()) {
      memDeps[{ i, j }][2] = memDep;
    }
  }

  /*
   * For each memory depedence perform loop-aware dependence analysis to
   * disprove it. Queries for loop-carried and intra-iteration deps.
   */
  for (auto memDep : memDeps) {

    /*
     * Fetch the current pair of instructions
     */
    auto instPair = memDep.first;
    auto i = instPair.first;
    auto j = instPair.second;

    /*
     * Fetch the dependences.
     */
    auto edges = memDep.second;

    // encode the found dependences in a bit vector.
    // set least significant bit for RAW, 2nd bit for WAW, 3rd bit for WAR
    uint8_t depTypes = 0;
    for (uint8_t i = 0; i <= 2; ++i) {
      if (edges[i]) {
        depTypes |= 1 << i;
      }
    }
    // Try to disprove all the reported loop-carried deps
    uint8_t disprovedLCDepTypes =
        disproveLoopCarriedMemoryDep(i, j, depTypes, l, NoelleSCAFAA);

    // for every disproved loop-carried dependence
    // check if there is a intra-iteration dependence
    uint8_t disprovedIIDepTypes = 0;
    if (disprovedLCDepTypes) {
      disprovedIIDepTypes = disproveIntraIterationMemoryDep(i,
                                                            j,
                                                            disprovedLCDepTypes,
                                                            l,
                                                            NoelleSCAFAA);

      // remove any edge that SCAF disproved both its loop-carried and
      // intra-iteration version
      for (uint8_t i = 0; i <= 2; ++i) {
        if (disprovedIIDepTypes & (1 << i)) {
          auto e = edges[i];
          loopDG->removeEdge(e);
        }
      }

      // set LoopCarried bit false for all the non-disproved intra-iteration
      // edges (but were not loop-carried)
      uint8_t iiDepTypes = disprovedLCDepTypes - disprovedIIDepTypes;
      for (uint8_t i = 0; i <= 2; ++i) {
        if (iiDepTypes & (1 << i)) {
          auto e = edges[i];
          e->setLoopCarried(false);
        }
      }
    }
  }
#endif

  return;
}

// TODO: Refactor along with HELIX's exact same implementation of this method
DataFlowResult *computeReachabilityFromInstructions(
    LoopStructure *loopStructure) {
  assert(loopStructure != nullptr);

  auto loopHeader = loopStructure->getHeader();
  auto loopFunction = loopStructure->getFunction();

  /*
   * Run the data flow analysis needed to identify the locations where signal
   * instructions will be placed.
   */
  auto dfa = DataFlowEngine{};
  auto computeGEN = [](Instruction *i, DataFlowResult *df) {
    assert(i != nullptr);
    assert(df != nullptr);
    auto &gen = df->GEN(i);
    gen.insert(i);
    return;
  };
  auto computeOUT = [loopHeader](Instruction *inst,
                                 Instruction *succ,
                                 std::set<Value *> &OUT,
                                 DataFlowResult *df) {
    assert(succ != nullptr);
    assert(df != nullptr);

    /*
     * Check if the successor is the header.
     * In this case, we do not propagate the reachable instructions.
     * We do this because we are interested in understanding the reachability of
     * instructions within a single iteration.
     */
    auto succBB = succ->getParent();
    if (succBB == loopHeader) {
      return;
    }

    /*
     * Propagate the data flow values.
     */
    auto &inS = df->IN(succ);
    OUT.insert(inS.begin(), inS.end());
    return;
  };
  auto computeIN =
      [](Instruction *inst, std::set<Value *> &IN, DataFlowResult *df) {
        assert(inst != nullptr);
        assert(df != nullptr);

        auto &genI = df->GEN(inst);
        auto &outI = df->OUT(inst);
        IN.insert(outI.begin(), outI.end());
        IN.insert(genI.begin(), genI.end());
        return;
      };

  return dfa.applyBackward(loopFunction, computeGEN, computeIN, computeOUT);
}

void refinePDGWithLIDS(PDG *loopDG,
                       LoopStructure *loopStructure,
                       LoopTree *loops,
                       LoopIterationSpaceAnalysis *LIDS) {

  /*
   * Compute the reachability of instructions within the loop.
   */
  auto dfr = computeReachabilityFromInstructions(loopStructure);

  std::unordered_set<DGEdge<Value, Value> *> edgesToRemove;
  for (auto dependency :
       LoopCarriedDependencies::getLoopCarriedDependenciesForLoop(
           *loopStructure,
           loops,
           *loopDG)) {

    /*
     * Do not waste time on edges that aren't memory dependencies
     */
    if (!isa<MemoryDependence<Value, Value>>(dependency)) {
      continue;
    }

    auto fromInst = dyn_cast<Instruction>(dependency->getSrc());
    auto toInst = dyn_cast<Instruction>(dependency->getDst());
    if (!fromInst || !toInst)
      continue;

    /*
     * Loop carried dependencies are conservatively marked as such; we can only
     * remove dependencies between a producer and consumer where we know the
     * producer can NEVER reach the consumer during the same iteration
     */
    auto &afterInstructions = dfr->OUT(fromInst);
    if (afterInstructions.find(toInst) != afterInstructions.end())
      continue;

    if (LIDS->areInstructionsAccessingDisjointMemoryLocationsBetweenIterations(
            fromInst,
            toInst)) {
      edgesToRemove.insert(dependency);
    }
  }

  for (auto edge : edgesToRemove) {
    edge->setLoopCarried(false);
    loopDG->removeEdge(edge);
  }

  /*
   * Free the memory
   */
  delete dfr;

  return;
}

NoelleSCAFIntegration::NoelleSCAFIntegration() : ModulePass{ ID } {
  return;
}

bool NoelleSCAFIntegration::doInitialization(Module &M) {
  return false;
}

void NoelleSCAFIntegration::getAnalysisUsage(AnalysisUsage &AU) const {
#ifdef ENABLE_SCAF
  AU.addRequired<liberty::LoopAA>();
  AU.addRequired<liberty::ModuleLoops>();
  AU.setPreservesAll();
#endif
  return;
}

bool NoelleSCAFIntegration::runOnModule(Module &M) {
#ifdef ENABLE_SCAF
  NoelleSCAFAA = getAnalysis<liberty::LoopAA>().getTopAA();
  ModuleLoops = &getAnalysis<liberty::ModuleLoops>();
  ModuleLoops->reset();
#endif

  return false;
}

std::set<AliasAnalysisEngine *> LoopContent::getLoopAliasAnalysisEngines(void) {
  std::set<AliasAnalysisEngine *> s;

#ifdef ENABLE_SCAF
  assert(NoelleSCAFAA != nullptr);
  auto aa = new LoopAliasAnalysisEngine("SCAF", NoelleSCAFAA);
  s.insert(aa);
#endif

  return s;
}

} // namespace arcana::noelle
