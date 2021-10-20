`repair-handles` is CLI that fixes inconsistencies between the tables `brig.user` and `brig.user_handle`.

This must be run to repair cassandra_brig after a bug fixed in https://github.com/wireapp/wire-server/pull/1303.  It fixes two kinds of inconsistencies:

1. A user for which `brig.user.handle` is `null` and there is exactly one row `<row>` in `brig.user_handle` matching the user.
   The tool fixes this by setting `brig.user.handle` to `<row>.handle`.
2. A user for which `brig.user.handle` is set to a value `<handle>` and there are exactly 2 rows `<row1>`, `<row2>` in `brig.user_handle` matching the user, and `<row1>.handle` equals `<handle>`.
   The tool fixes this by setting `brig.user.handle` to `<row2>.handle` and deleting `<row1>.handle` from `brig.user_handle`.
