# Writing code interacting with cassandra

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
