# Task 8 report
- Added Metrics.hs, MetricsSpec.hs verbatim from brief; cabal module lists updated.
- TDD: first build failed (no Eq on SampleType in `ty shouldBe HistogramType`); applied brief's fallback `case ty of`. Test-suite also needed `http-types` dep (not in brief).
- Result: 54 examples, 0 failures; no warnings; ormolu clean.
- default.nix regenerated (http-types test dep).
- Commits: e48fa1441 (feature), follow-up nix regen.
