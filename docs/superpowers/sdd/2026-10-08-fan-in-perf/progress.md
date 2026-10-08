# SDD ledger — plan: docs/superpowers/plans/2026-10-08-fan-in-perf.md
Spec: docs/superpowers/specs/2026-10-08-fan-in-perf-design.md (reachable)

## Preflight scan
| Pair/Task | Produces vs consumes | Finding |
|---|---|---|
| T1 / T2 | T1 Postgres.hs (isolation param); T2 new admin modules, both edit wire-subsystems.cabal (different sections) | ok |
| T1 / T10 | T1 `interpretFanInNotificationsStoreToPostgres :: IsolationLevel -> ...`; T10 Store.hs calls it | ok |
| T2 / T10 | T2 TruncateAll/Ping; T10 reset + startup probe | ok |
| T3 / T10 | T3 initPostgresPoolFromConnString; T10 Store.hs | ok |
| T4 / T5 | T4 Targets parser; T5 extends same file+spec | ok |
| T5 / T10 | mkPush/genTargets consumed by writerStep | ok |
| T6 / T7,T8,T10 | Stats snapshot/tick consumed by Terminal, Metrics, Produce | ok (types in plan) |
| T7,T8,T9 | each adds modules to fan-in-perf.cabal | ok, sequential |
| T9 / T10 | Options consumed by Run/Main | ok |
| T11 / T8 | port 9400 + metric names in dashboard | ok |
| T12 | gates; acceptance needs user DB + schema regen (user task) | handoff |
| Each task self-consistent | code vs tests spot-checked at plan self-review | ok |

Ruling: commit trailer = `Co-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>` (system attribution), overriding plan's "Opus 5.5" — cosmetic; fixable by rebase.
Ruling: CLAUDE.md filtering — implementers must filter build output (grep -vE ...). 
Task 1: dispatched implementer a91069ba343923e07 (BASE ad601d688)
Task 1: complete (commits ad601d6..1edb499, review clean)
Note: build env needs 'export LIBRARY_PATH=$PWD/.env/lib LD_LIBRARY_PATH=$PWD/.env/lib' (direnv blocked). Handoff for user: make postgres-reset && make postgres-schema (docker) after Task 1.
Task 2: complete (commits 1edb499..f36646d, review clean)
Task 2: minor (deferred): Ping uses noResult on SELECT 1 — unverified vs live PG, confirm in T12 acceptance run
Task 2: minor (deferred): TruncateAll list not auto-synced with migration
Task 3: implementer done 08ec211, in review
Task 3: complete (commits f36646d..08ec211, review clean)
Task 4: implementer done e1595b7, in review
Task 4: complete (commits 08ec211..e1595b7, review clean)
Task 4: minor (deferred): library build-depends pre-populated for later tasks; final review check for unused deps
Task 5: implementer done 7c49ebc, in review
Task 5: complete (commits e1595b7..7c49ebc, review clean)
Task 5: minor (deferred): mkEntries clientsPerUser<=0 silently 1 — validate in Options (T9)
Task 6: complete (commits 7c49ebc..42f81fa, review clean)
Task 6: minor (deferred): p50/p99 cumulative incl. warmup; bucketIndex property weak coverage (uniform gen), 2^39 untested; async test-dep nix alignment -> T12 regen
Task 7: implementer done 8eeb108, in review
Task 7: complete (commits 42f81fa..8eeb108, review clean)
Task 7: minor (deferred): printLine/drawStatus unsynchronised — ticker-only use; T10 final summary must use printLine (TTY has no trailing newline)
Task 8: implementer done e48fa14..14beffd, in review
Task 8: complete (commits 8eeb108..14beffd, review clean)
Task 8: minor (deferred): counter series appear only after first nonzero delta (dashboard panels use rate(); T11 check); le rendered via show (2.0e-9)
Task 9: implementer done dcfe620, in review
Task 9: complete (commits 14beffd..dcfe620, review clean)
Task 9: minor (deferred): negative-number tests may pass via optparse unknown-option rather than reader; no warmup<duration check
Task 10: implementer done e4a990b, in review
Task 10: review: spec ✅, 1 Important (final doTick noisy dt inflates maxPushRate)
Ruling: also fix lazy genTargets counted in push latency (Minor) in same round — it distorts the benchmark's core measurement — cost if wrong: trivial extra diff
Task 10: fix round 1/5 dispatched (resumed implementer; FIX_BASE e4a990b)
Task 10: minor (deferred): Store.hs:55 SessionError shown in full (SQL+params, KBs/line); second Ctrl-C during shutdown loses summary; ticker cancel between writeIORef and publish drops a delta; missing tests: async exc passthrough in writerStep, describeUsageError only ConnectionError
Task 10: fix round 1/5 (2 addressed, 0 open; commits e4a990b..58315a1)
Task 10: complete (commits dcfe620..58315a1, review clean after 1 fix round)
Task 11: implementer done 64d1969, in review
Task 11: review spec ✅; 1 Important (rate windows [15s] vs 5s scrape; plan-mandated)
Ruling: keep [15s] windows — plan mandated, 3 samples per window suffices for rate(); only jitter risks gaps; cost if wrong: gappy panels, 1-line edit to [30s]/$__rate_interval
Task 11: complete (commits 58315a1..64d1969, review clean, 1 Ruling)
Task 11: minor (deferred): datasource null; OTLP round-trip name translation unverified -> confirm in T12 / user Grafana Explore
Task 12 (steps 1-5): complete (commit f9704fe; acceptance run step 6 pending user schema confirmation)
Final review: 3 Important (stale postgres-schema.sql = user handoff; no changelog.d; deadlock lock-order) + minors
Ruling: deadlock fixed tool-side (sort in genTargets), not in store — avoids changing RFC-modelled store; store unsorted order is a finding to report in spike — cost if wrong: store still deadlock-prone in prod multi-target pushes
Ruling: bundled reviewer minors (SessionError truncation, Integer-bounded numeric flags, p50/p99 labelled cumulative, targets_total doc, cheaper force) into the single fix wave
Final: fix wave dispatched (resumed a27c5f8; FIX_BASE f9704fe)
Final: fix wave complete (commit 71d5c78), scoped re-review all ADDRESSED, no new breakage. Acceptance run (T12 step 6) pending user schema regen.
