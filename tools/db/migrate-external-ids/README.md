This tool fills entries in the table `spar.scim_external` (introduced in release [2021-02-16](https://github.com/wireapp/wire-server/releases/tag/v2021-02-16)) from table `spar.scim_external_ids` and `brig.user`. It is meant to be used in the migration from `spar.scim_external_ids` to `spar.scim_external`.

Next steps after this migration: spar will stop using `spar.scim_external_ids` once `spar.scim_external` is fully populated.  After that, `spar.scim_external_ids` can be removed.
