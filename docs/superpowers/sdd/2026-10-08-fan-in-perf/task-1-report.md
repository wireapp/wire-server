# Task 1 report
- Pre-refactor: genNotificationId spec 2/2 PASS (v7, strictly increasing) against old impl.
- Changes: migration group_id bytea (4 tables); interpreter takes IsolationLevel; genNotificationId = Id <$> UUIDv7.genUUID; epoch stmts use bytea/ByteString; unused imports removed; spec + cabal other-modules.
- No other callers of interpretFanInNotificationsStoreToPostgres in repo.
- Post: wire-subsystems-tests 612 examples, 0 failures, 6 pending; no warnings. ormolu applied.
- Env note: cabal needed LIBRARY_PATH/LD_LIBRARY_PATH=$PWD/.env/lib (direnv blocked).
- Hand-off: schema changed -> `make postgres-reset`, `make postgres-schema` (docker; not run).
