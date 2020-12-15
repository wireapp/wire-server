# move-team - export/import teams

The executable `move-team` can..
- ... export all data that belongs to a team to a file dump
- ... import files json files to cassandra
- ... export all data from some tables. This was only intended for development.

The script `debug_merge_teams.sh` can be used to replace all team ids in a file
with a a single team id. This can be useful for testing.

`src/Schema.hs` is created by the executable `gen-table-types`.
You can also use `ParseSchema.debugwrite` to recreate it from a ghci.
It parses the cql `docs/reference/cassandra-schema.cql`

Note:
  `move-team` has not been thoroughly tested yet.
  Look for FUTUREWORKs in the code.
