# Task 10 report
Status: DONE. Added Store, Produce, Run, Main, README, ProduceSpec; cabal module lists.
Tests: 76 examples, 0 failures, no warnings. ProduceSpec (writerStep x3 incl. non-UsageError exception, describeUsageError no leak) written alongside; compile-fail-first not separately captured.
Compile fixes vs brief: Imports already exports STM -> import only Control.Exception (UserInterrupt, handleJust); removed unused Data.List; `wait` shadow -> `sleepFor`; Data.List.NonEmpty (NonEmpty) imported; spec: `hiding (spec)` for Targets in ProduceSpec; ErrorCall instead of userError/throwIO import from Control.Exception.
Review focus: exceptions counted via tryAny (async propagates); describeUsageError generic; invalid --db not echoed (smoke: prints generic msg, exit=1); short run/early Ctrl-C: tick guards dt<=0, final doTick + summary via printLine; ticker only thread printing (final tick after withAsync cancelled it); pool size Maybe -> writers/1.
CLI --help and produce --help checked. No DB connection, no cabal dep change (no nix regen).

## Fix round 1
1. Stats.tick: max push rate only updated when dt >= minTickInterval (0.5s), so the tiny final tick cannot set the headline max; totals/summary unaffected. New StatsSpec test "a tiny final tick counts in totals but does not raise the max rate".
2. Produce.writerStep: targets forced (evaluate over targetKey lengths, deep enough to build ids) before t0, so latency covers only the store call. No new test (timing).
Command: make c package=fan-in-perf test=1 | grep ... -> 77 examples, 0 failures, no warnings. Commit message trailer Sonnet 5.5.

## Fix wave (final review)
1. Targets.genTargets sorts sampled indices (global pool order; client ids already ascending). Test: TargetsSpec "targets within a push are in ascending pool order". README note.
2. changelog.d/5-internal/WPB-26288-fan-in-perf added.
3. Store.truncateText (200 chars + ellipsis) used for SessionError; ProduceSpec truncateText tests (SessionError value itself not constructed).
4. Options: Integer-based bounded readers; max pool-size/writers 10_000, clients-per-user 100_000, payload 10_000_000, duration/warmup 315_360_000, port via same. OptionsSpec rejects 18446744073709551617 etc.
5. Status/summary p50/p99 labelled cumulative; TerminalSpec updated; README.
6. Metrics help + README: clients targets count users.
7. Targets.forceTargets (strict traversal) replaces T.length . targetKey, before t0.
Command: make c package=fan-in-perf test=1 -> 88 examples, 0 failures, no warnings. ormolu check ok. make lint-all aborts at formatc ("Working copy is not clean", unrelated to code); not run past that.
