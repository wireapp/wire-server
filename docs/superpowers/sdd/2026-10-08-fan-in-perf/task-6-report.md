# Task 6 report
Status: DONE (with deviations below).
- Implemented Stats.hs + StatsSpec.hs verbatim from brief; cabal updated (Stats module, StatsSpec, async in test deps).
- TDD evidence: files were written together, then first build failed: `RealWorld` not in scope (brief imports omitted it). Fix: added `RealWorld` to the `GHC.Exts` import. Not a separate red run of tests-only (deviation).
- First test run: 1 failure, prop "latency is below its bucket's upper bound" gave up (1000 discarded; random Word64 almost never < 2^39, `==>` precondition). Fix (test only): `forAll (choose (1, 2^(numBuckets-1) - 1))`. Property semantics unchanged.
- Final: 42 examples, 0 failures, no warnings. ormolu applied.
- dt=0 / no traffic: rate guards dt>0, errorRatio guarded, quantile Nothing, max 0: tested, passes.
- Concern: maxPushRate uses `elapsed > warmup` on tick end time (as in brief), so the tick straddling warmup end counts.
