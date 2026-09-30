Migrated the Elasticsearch client library from our `wireapp/bloodhound` fork to
upstream `bitemyapp/bloodhound` v1.0.0.0 (WPB-12109). Elasticsearch is now
accessed through the untyped APIs (`/{index}/_doc/...`, `PUT /{index}/_mapping`),
which requires Elasticsearch >= 7 or >= 6.7 with indices that have no legacy
document type. User documents are now versioned with `external_gte` instead of
strictly-greater `external` versioning: re-indexing a document with an equal
version now overwrites it instead of being skipped. ##
