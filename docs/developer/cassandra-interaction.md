# Writing code interacting with cassandra

<!-- vim-markdown-toc GFM -->

* [Anti-patterns](#anti-patterns)
    * [Anti-pattern: Using full table scans in production code](#anti-pattern-using-full-table-scans-in-production-code)
    * [Anti-pattern: Using IN queries on a field in the partition key](#anti-pattern-using-in-queries-on-a-field-in-the-partition-key)
    * [Anti-pattern: Designing for a lot of deletes or updates](#anti-pattern-designing-for-a-lot-of-deletes-or-updates)
* [Understanding more about cassandra](#understanding-more-about-cassandra)
    * [primary partition clustering keys](#primary-partition-clustering-keys)
    * [optimizing parallel request performance](#optimizing-parallel-request-performance)
* [Cassandra schema migrations](#cassandra-schema-migrations)
    * [Backwards compatible schema changes](#backwards-compatible-schema-changes)
    * [Backwards incompatible schema changes](#backwards-incompatible-schema-changes)
        * [What to do about backwards incompatible schema changes](#what-to-do-about-backwards-incompatible-schema-changes)

<!-- vim-markdown-toc -->

## Anti-patterns

### Anti-pattern: Using full table scans in production code

Queries such as `select some_field from some_table;` are full table scans. Cassandra is not optimized at all for such queries, and even with a small amount of data, a single such query can completely mess up your whole cluster performance. We had an example of that which made our staging environment unusable. Luckily, it was caught in time and [fixed](https://github.com/wireapp/wire-server/pull/1574/files) before making its way to production.

Suggested alternative: Design your tables in a way to make use of a primary key, and always make use of a `WHERE` clause: `SELECT some_field FROM some_table WHERE some_key = ?`.

In some rare circumstances you might not easily think of a good primary key. In this case, you could for instance use a single default value that is hardcoded: `SELECT some_field FROM some_table WHERE some_key = 1`. We use this strategy in the `meta` table which stores the cassandra version migration information, and we use it for a [default idp](https://github.com/wireapp/wire-server/blob/4814afd88b8c832c4bd8c24674886c5d295aff78/services/spar/schema/src/V7.hs). `some_field` might be of type `set`, which allows you to have some guarantees. See the implementation of unique claims and the [note on guarantees of CQL
sets](https://github.com/wireapp/wire-server/blob/develop/services/brig/src/Brig/Unique.hs#L110) for more information on sets.

### Anti-pattern: Using IN queries on a field in the partition key

Larger IN queries lead to performance problems. See https://lostechies.com/ryansvihla/2014/09/22/cassandra-query-patterns-not-using-the-in-query-for-multiple-partitions/

A preferred way to do this lookup here is to use queries operating on single keys, and make concurrent requests. One way to do this is with the [`pooledMapConcurrentlyN`] (https://hoogle.zinfra.io/file/root/.stack/snapshots/x86_64-linux/e2cc9ab01ac828ffb6fe45a45d38d7ca6e672fb9fe95528498b990da673c5071/8.8.4/doc/unliftio-0.2.13/UnliftIO-Async.html#v:pooledMapConcurrentlyN) function. To be conservative, you can use N=8 or N=32, we've done this in other places and not seen problematic performance
yet. For an optimization of N, see the section further below.

### Anti-pattern: Designing for a lot of deletes or updates

Cassandra works best for write-once read-many scenarios.

Read e.g.
- https://www.datastax.com/blog/cassandra-anti-patterns-queues-and-queue-datasets
- https://www.instaclustr.com/support/documentation/cassandra/using-cassandra/managing-tombstones-in-cassandra/#
- search the internet some more for 'cassandra' and 'tombstones' and if you find good posts, add them here.

## Understanding more about cassandra

### primary partition clustering keys

Confused about primary key, partition key, and clustering key? See e.g. [this post](https://blog.devgenius.io/cassandra-primary-vs-partitioning-vs-clustering-keys-3b3fa0e317f4) or [this one](https://dzone.com/articles/cassandra-data-modeling-primary-clustering-partiti)

### optimizing parallel request performance

See the thoughts in https://github.com/wireapp/wire-server/pull/1345#discussion_r567829234 - measuring overall and per-request performance and trying out different settings here might be worthwhile if increasing read or write performance is critical.

## Cassandra schema migrations

### Backwards compatible schema changes

Most cassandra schema changes are backwards compatible, or *should* be designed to be so. Looking at the changes under `services/{brig,spar,galley,gundeck}/schema` you'll find this to be mostly the case.

The general deployment setup for services interacting with cassandra have the following assumption:

* cassandra schema updates happen *before* new code is deployed.
  * This is safeguarded by the concourse deployment pipelines
  * This is also safeguarded by the `wire-server` helm chart, which deploys the `cassandra-migrations` job as part of a helm [pre-install/pre-upgrade hook](https://github.com/wireapp/wire-server/blob/b3b1af6757194aa1dc86a8f387887936f2afd2fb/charts/cassandra-migrations/templates/migrate-schema.yaml#L10-L13): that means the schema changes are applied, and helm waits before launching the new code in the brig/galley/spar/gundeck pods until the changes have completed applying.
  * This is further safeguarded by the code e.g. in brig refusing to even start the service up if the applied schema migration is not at least at a version the code expects it to. See [versionCheck](https://github.com/wireapp/wire-server/blob/b3b1af6757194aa1dc86a8f387887936f2afd2fb/services/brig/src/Brig/App.hs#L411) and [schemaVersion](https://github.com/wireapp/wire-server/blob/b3b1af6757194aa1dc86a8f387887936f2afd2fb/services/brig/src/Brig/App.hs#L136-L137)

So usually with these safeguards in place, and backwards-compatible changes, we have the following:

* At time t=0, old schema, old code serves traffic; all good.
* At time t=1, new schema, old code serves traffic: all good since backwards compatible.
* At time t=2, new schema, old code AND new code serve traffic: all good since backwards compatible.
* At time t=3, new schema, new code serves traffic: all good since backwards compatible.

If this order (apply schema first; then deploy code) is not safeguarded, then there will be code running in e.g. production which `SELECT my_new_field FROM my_new_table` even though this doesn't yet exist, leading to 500 server errors for as long as the mismatch between applied schema and code version persists.

### Backwards incompatible schema changes

In the case where a schema migration is **not backwards compatible**, such as in the form of `ALTER TABLE my_table DROP my_column`, the reverse problem exists:

During a deployment:

* At time t=0, old schema, old code serves traffic; all good.
* At time t=1, new schema, old code serves traffic: 500 server errors as the old code is still active, bit columns or tables have been deleted, so old queries of `SELECT x from my_table` cause exceptions / HTTP 5xx results.
    * In the worst deployment scenario, this could raise an alarm and lead operators or automation to stop the deployment or roll back; but that doesn't solve the issue, 500s will continue being thrown until the new code is deployed.
* At time t=2, new schema, old code AND new code serve traffic: partial 500s (all traffic still serves by old code)
* At time t=3, new schema, new code serves traffic: all good again.

#### What to do about backwards incompatible schema changes

Options from most to least desirable:

* Never make backwards-incompatbile changes to the database schema :)
* Do changes in a two-step process:
    * First make a change that stops the code from using queries involving `my_column` or `my_table` (assuming you wish to remove those), and deploy this all the way across all of your environments (staging, prod, customers, ...).
    * After all deployments have gone through (this can take weeks or months), do the schema change removing that column or table from the database. Often there is no urgency with removal of data, so it's fine to do this e.g. 6 months later.
* Do changes in a one-step process, but accept partial service interruption for several minutes up to a few hours, accept communication overhead across teams to warn them about upcoming interruption or explain previous interruption; accept communication and documentation to warn all operators deploying that or a future version of the code, and accept some amount of frustration that may arise from a lack of said communication and understanding of what is happening.
