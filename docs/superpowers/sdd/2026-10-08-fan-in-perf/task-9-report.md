# Task 9 report
Status: DONE. Options.hs + OptionsSpec.hs added, cabal module lists updated. No new deps (nix unchanged).
Deviations from brief (compile fixes):
- Options.hs: `import Options.Applicative hiding (command)` + qualified `O.command` (clash with record field `command`); added `import Data.List.NonEmpty (NonEmpty)`.
- Spec: parseOk helper (per brief note); Targets import restricted to `TargetEntry (..), TargetKind (..)` (Entry.spec field clashed with `spec`).
- Added rejection tests: clients-per-user 0/-1, writers -1, payload-bytes 0, duration 0, warmup -1, pool-size 0. (Brief's `positive` already rejects <1, covers deferred Targets note.)
- GlobalOptions has no Show.
Tests: 72 examples, 0 failures; no warnings.
