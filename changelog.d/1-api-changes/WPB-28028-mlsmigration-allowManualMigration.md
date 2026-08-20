The `mlsMigration` team feature config now includes an `allowManualMigration`
boolean field (default `false`) that controls whether clients are permitted to
perform single-group (manual) MLS migrations. The field only steers client
behaviour (e.g. if a migration button is shown or not). It does not enforce
checks in the backend.
