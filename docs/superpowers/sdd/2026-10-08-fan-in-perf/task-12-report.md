# Task 12 steps 1-5 report
Env: export LIBRARY_PATH=$PWD/.env/lib LD_LIBRARY_PATH=$PWD/.env/lib. Steps 6-7 skipped (no DB/docker).

1. `make format` -> all files ok, git status unchanged (PASS; no refusal on untracked-only tree).
2. `make lint-all`: first run hlint hint tools/fan-in-perf/test/FanInPerf/TerminalSpec.hs:57 (Hoist not) -> fixed to `(not . any (T.isInfixOf "NaN"))`. After commit, `make lint-all` exit 0 (formatc, hlint "No hints", treefmt 0 changed, check-local-nix-derivations ok). (It failed once pre-commit only due to dirty-tree nix check.)
3. Unused deps: manual import audit; removed `hasql` and `stm` from library build-depends (no imports; test suite needs hasql for Hasql.Errors, kept). Package builds+tests pass without them. `make regen-local-nix-derivations` -> tools/fan-in-perf/default.nix drops stm; no other diff. Cabal/nix aligned.
4. `make c` -> Up to date, no warnings/errors. `make c package=wire-subsystems test=1` -> 612 examples, 0 failures, 6 pending. `package=fan-in-perf test=1` -> 77 examples, 0 failures. `package=extended test=1` -> 24 examples, 0 failures.
5. Commit f9704fe62 "fan-in-perf: hlint fix, drop unused deps (hasql, stm)".
