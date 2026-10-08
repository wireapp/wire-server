# Task 4 report
Commit e1595b779. 26 examples, 0 failures; no GHC warnings.
TDD: with tests but no Targets module, `cabal build` failed "can't find source for FanInPerf/Targets". Then implemented; green.
Deviations:
- cabal: added `flag nix-dev-env` + `if !flag(nix-dev-env)` guard for hspec-discover build-tool-depends (as wire-subsystems does), and `package fan-in-perf / flags: +nix-dev-env` in cabal.project. Without it the solver failed for whole project.
- Imports lacks `NonEmpty` type/`nonEmpty`: imported from Data.List.NonEmpty. Added `malformed :: Either String a` signature (monomorphism error).
- Extra tests: maxStreams accepted, `user:50000000`, 20-digit numbers, exact error message for absurd number.
- Parsing via Integer first, bound 1..10_000_000: no overflow/allocation.
- make regen-local-nix-derivations ran fine (generated default.nix + local-haskell-packages entry).
- ormolu: no changes; `make format` refuses on dirty tree, ran ormolu directly.
