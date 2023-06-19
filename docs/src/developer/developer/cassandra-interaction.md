# Writing code interacting with cassandra

## Background on "why cassandra?"

Cassandra was chosen back in ~2014 in the context of massive scalability (expectations of billions
of users), as well as the wish for redundancy and cloud-always-on high-availability.

From an operational standpoint, high-availability and redundancy are well served with this: any
single node/VM can crash at any moment without data loss, and version upgrades, server maintenance,
and server migrations can be done without stopping the system (and causing user-observable
downtime).

From a developer perspective, wishing for a mental model of strong consistency across multiple
tables in multiple services to make easier assumptions when writing code makes working with
cassandra sometimes a bit harder. (That said, this document has some sections below to make working
with strong consistency within the same service easier).

Questions have come up whether another technology may serve our needs better in the evolved
technical and product context of 2023 and beyond. These questions are valid, and a change can always
be discussed, if the time is at hand to put this effort in.

## Guidelines for designing database schemas

In a lot of relational databases, you design tables with their relations, and think about the exact
access patterns later, as you can always add an index on a certain column. In the cassandra world,
you need to make some mental gymnastics and get used to design your tables **by the access patterns
to this data**. You can only look up data by what's in the primary key, and you always need the
single partition key at hand to do this lookup.

Therefore, you may need multiple tables that relate, such as a `conversation_by_user_id` and a
`user_by_conversation_id` table (or even more of them). That's perfectly fine and the correct
pattern to use. Please read some online resources about data modeling with cassandra, such as the
[datastax official data modelling documentation](https://cassandra.apache.org/doc/latest/cassandra/data_modeling/index.html).

## Guidelines on per-table query consistency level and replication level

### Background

We currently operate all our production and production-like environments with

* a **replication factor of 3** (per datacentre, but except for a brief phase of controlled
  datacentre migration there is only one datacentre in use)
* a number of cassandra nodes >= 3 (depending on load and disk/machine size)
* Everywhere where some consistency matters, **Quorum writes and Quorum reads**

These three things together give us **strong consistency on a per-table** basis. *(well, actually,
it's monotonic read consistency we get here - concurrent requests may still see the old value until
the quorum write finishes. In practice, you most likely do not need to care about that difference in
the Wire context though.)*. A quorum write followed by a quorum read will guarantee that you see the
data you inserted, even if one replica of cassandra that holds that partition key dies in the
middle.

The replication factor is specified when creating or migrating schemas, which is done in the
`cassandra-migrations` subchart of the `wire-server` chart:

```{grepinclude} ../charts/cassandra-migrations/values.yaml host name and replication
---
lines-after: 6
language: yaml
---
```

The number of cassandra nodes in use is specified on the infrastructure level (k8ssandra on
kubernetes, or the inventory list when using ansible-cassandra)

Quorum consistency (or, in our case, `LocalQuorum` consistency) is specified in our code. Random
example:

```{grepinclude} ../services/brig/src/Brig/Data/User.hs userEmailUpdate \(params
---
language: Haskell
---
```

Note that `Quorum` and `LocalQuorum` behave exactly the same in the context of a single datacentre
(each datacentre can have multiple racks or availability zones). We switched from `Quorum` to
`LocalQuorum` in [this PR](https://github.com/wireapp/wire-server/pull/1884) in preparation of a
datacentre migration. As it doesn't hurt, and more datacentre migrations may be done in the future
by us or the people hosting on-premise, let's stick to `LocalQuorum` for the time being.

***To summarize: In case of doubt, just follow the examples and use `LocalQuorum` Writes and Reads
in your queries, unless there is a very good reason not to.***

(batch-statements)=
## Guidelines for across-table (atomic) consistency

Given that we have strong consistency for a single operation (see section above), in some cases your
data access patterns mandate that you have multiple tables with seemingly duplicated data.

Good ✅ example: For instance, a 'teamInvitation' needs to be queried
* by team ID (for the team admin- which pending invitations exist?)
* by email (during user registration), and
* by code (to check whether an invitation is valid before embarking on the user registration).

Therefore, there are three tables holding similar data about a team invitation, but with different
primary keys. This is a correct and recommended data design for cassandra.

Now, any insert, update or delete of this data should either all work, or all fail, to not have
weird data integrity issues. Now, cassandra provides for this, and it's called `batch` statements.

> Batches are atomic by default. In the context of a Cassandra batch operation, atomic means that if
> any of the batch succeeds, all of it will.

> Statement order does not matter within a batch

[reference documentation of batch](https://docs.datastax.com/en/archived/cql/3.1/cql/cql_reference/batch_r.html)

Here's an example of this used in our code:

```{grepinclude} ../services/brig/src/Brig/Team/DB.hs insertInvitation showUrl
---
language: Haskell
lines-after: 18
---
```

While writing this documentation, I grepped for batch across our codebase, and saw that the use of
batch statements seems to have fallen into disuse over the past months and years. That's a mistake!
For related data, batch statements should be used to keep data integrity across multiple tables
for related data.

*Sidenote:* Batch statements have a confusing name perhaps. They are not intended to speed up
inserting multiple records into the same table; that is an antipattern, see [this
documentation](https://docs.datastax.com/en/cql-oss/3.x/cql/cql_using/useBatchBadExample.html).

*Sidenote 2:*: Batch statements do not have the I for isolation of an ACID transaction. While
writing to table A and B of a batch, another process might read table A and B and already see the
update in A but not see the update in B yet. As this is a transient problem, and in practice in the
wire-server codebase most lookups read only table A *or* table B and not both concurrently (as often
only one primary key needed for a lookup is available in context of a http request leading to
lookup), I propose to ignore this by default. At least so far using batch statements hasn't bitten
us / caused any bugs that I'm aware of. Don't let this scare you off - *not* using batch statements
is worse, as there you're guaranteed to get long-lasting data inconsistencies across tables.

***To summarize: When working on multiple tables with related data (e.g. user_by_conversation_id and
conversation_by_user_id), use 'batch' statements to achieve atomicity! When inserting lots
of data into the same table, do not use batch statements.***

## The case for more 'batch' statement usage

Adding to the section before, an example taken out of thin air to argue for more usage of `batch`
statements:

*Example: Let's say there's a new Wire chat, and I add you to this chat X. In the database we have
one table users_by_conversation with primary key conversationId, and one table
conversations_by_users with primary key userId.*

Then, there are two ways of writing code for that:

* way 1: use a batch statement to insert the user->conv and conv->user entries of chatX-User-A and
User-A-chatX.
* way 2: insert these rows separately in some order of your choosing.

Now, if the Haskell process crashes midway, then we can have these outcomes:
- with way 1: either the write succeeded or it didn't. So either User-A is in chat X (way-1-A) or
  they are not (way-1-B). But either way it's in both tables or in none, depending on when the crash
  happened.
- with way 2: there's a chance one write to one table went through but the other didn't.

So with way-1-A - 5 minutes later - both User-A looking at their conversation list sees chat X
appear; and other members of chat X looking at the member list of that chat see User-A as a
member. With way-1-B: User-A doesn't see chat-X and other members of the chat don't see User-A
as a member. With way-2: It's inconsistent: User-A sees the chat, but other people can't see
him, or when accessing the chat some error occurs, and it's weird. Or he's in the chat but
doesn't see the chat appear in their UI and it's weird.

Now, way-1-B can be retried. I can try and add User-A again to chat X.

That there is no isolation as the I in [ACID](https://en.wikipedia.org/wiki/ACID) - well, here I
don't care. Anyway we have a race condition: User-B refreshing the member list of chat X will see no
User-A until t=`<addition-time>` and thereafter sees User-A appear. That in theory there is a time gap
between the writes of two tables where user-A is in one table but not the other doesn't matter for
User-B, it's a state change from not-in-chat to in-chat. Similarly for User-A himself, it's a state
change from not-in-chat to in-chat, the access pattern of reading the member list in a weird order
isn't relevant here. At worst a refresh will show the new state of things.

But way-2 is terrible! It will forever remain weird and appear as a bug either for User-A, or
all other users in that chat, or all of us, and not resolve. In this case a batch statement is
**miles** better from a user perspective in the Wire context.

## Lightweight transactions

In some cases, it is important to make sure that a write is conditional to the existence (or non-existence) of a record. One example is uploading the public MLS signature key for a client: only one such key should be uploaded (per supported ciphersuite), so any attempts to write a new key when one is already present should fail.

This sort of "compare-and-swap" semantics can be achieved using Cassandra's lightweight transaction (LWT) mechanism. One can include the condition `IF EXISTS` or `IF NOT EXISTS` in a write query, which can then be executed using the `trans` function from the Cassandra Haskell driver. It is important to set `serialConsistency` to `LocalSerialConsistency` on the `QueryParams` structure passed to `trans`, since that is required for Cassandra to take into account other concurrent lightweight transactions when reading the value initially.

The result of a ligthweight transaction is a single row containing a boolean value, which indicates whether the update was successful.

Here is the code corresponding to the above example:

```{grepinclude} ../services/brig/src/Brig/Data/Client.hs addMLSPublicKey ::
---
language: Haskell
lines-after: 22
---
```

## Examples of code design for optimal user-level consistency in complex scenarios

### Introduction

As we saw above, we can achieve strong consistency for a single table, and for related tables in the
same database, if the code path allows grouping inserts/updates/deletes together. In some scenarios,
where some data is held by brig and some data is held by galley, this isn't easily achievable, and
we need to think harder abour ordering and possibly apply some tricks. There's no easy answer to
always have strong consistency, so instead here are some examples to guide future complex scenarios:

### Creating a user account

Upon creating a user, some resources are created by brig, some by galley (such as a self
conversation). Since LocalQuorum+batch statements are not an option here, and the services can crash
mid-way their request, we still wish to ensure that no inconsistent half-written data leads to a
broken experience (we can accept garbage data lingering around, but we don't wish to accept a user
being able to log in but having a broken state. Instead, we want to rely on some (in this case,
user-level) retry. To achieve this, there is a boolean flag 'activated'. In the past, what happened was:

* write a user entry with activated=false
* perform other operations in brig and galley and elsewhere
* update the user entry and set activated=true
* when doing a user lookup, filter out any results with activated=false (or null)

This way, if the code fails halfway through, even though there might be some data in the database,
as it is being filtered out, the user can retry the account creation and is not left in a
halfway-there broken state. This is not "pretty", but works for practial purposes.

The code around this has been refactored and there are many scenarios of user creation, so the
initial trick explained above may not hold true in all cases (thus potentially having broken the
desired data integrity properties).

### Deleting a user account

When deleting a user, also handles, clients, conversations, assets etc must be deleted.

To guard against this failing midway due to a crashing/restarting service, the `deleteAccount`
function is used in a "automated retry-logic" way. For this, it's wrapped with some queue logic
(push "this user should be deleted" onto a queue, start deleting things and only remove that item
from the queue once all has completed).

The function in question:

```{grepinclude} ../services/brig/src/Brig/API/User.hs deleteAccount account
---
language: Haskell
lines-after: 20
lines-before: 20
---
```

The queue logic:

```{grepinclude} ../services/brig/src/Brig/API/User.hs deleteUserNoVerify uid
---
language: Haskell
lines-after: 2
lines-before: 0
---
```

and

```{grepinclude} ../services/brig/src/Brig/InternalEvent/Process.hs DeleteUser uid
---
language: Haskell
lines-after: 4
lines-before: 0
---
```

Also, default SQS queue settings mean that events are only removed from the queue after the client
code finishes processing. If the client fails mid-way, the event re-appears for consumption (after a
timeout passes).

### Designing for consistent data

As we saw above, within a single batch request we can guarantee strong consistency for a single
service. If possible, try to not put some data in brig and some data in galley, as these cases will
make it hard to stay consistent. Instead, think about merging the services, or the database access,
to allow you to make use of batch queries. Or, try to keep related data within one service.

### The future: Cassandra 5.0 and true ACID

Right now, we have `batch` statements (see above), which are atomic, which is usually good enough.
But the 'i' for isolation from an ACID perspective is missing, so in a race condition another query
from another thread could read one of the table updates but not the other. Usually that's not a
problem though.

See [this post](https://thenewstack.io/an-apache-cassandra-breakthrough-acid-transactions-at-scale/)
on full ACID support coming to cassandra in the future. Note these true transactions will also not
work if one piece of your data is in galley and the other is in brig. They wouldn't in a postgres
scenario either. So, first steps first: move your data together!

## Guidelines for deciding where correctness and consistency matters most, and where it's okay to be inconsistent

FUTUREWORK.

## Guidelines for taking decisions on performance

FUTUREWORK.

(discover-and-repair)=
## We made a mistake. How to detect data inconsistency across some tables?

This is a little time consuming to do. It involves writing a kind of script which does a paginated
full table scan on one or more table and compares (and possibly repairs) data. We have some examples
like this, see <https://github.com/wireapp/wire-server/tree/develop/tools/db/inconsistencies>. A
on-access ad-hoc detection/repair can in some cases also occur during normal code path execution if
you know/suspect data integrity problems from previous buggy code that was deployed.

## Anti-patterns, pain points and gotchas

### Gotcha: update x set y=z WHERE id=<id>

Note that `UPDATE` statements really are exactly the same as `INSERT` statements, and a `update
my_table set y = z WHERE id=<id>` statement will **insert** a row with these values even if `<id>`
does not yet exist. Knowing this, you can opt for read-before-write, use a LWT, or program
defensively with the knowledge that this may create some rows where not all columns have a value.

You may wish to always use `INSERT` statements, or beware of this confusion and add some comments to
your code to prevent your co-workers from incorrectly using the update statements.

### Pain point: inconsistent data

> My biggest pain point so far was hunting reasons for users having inconsistent data.

Most likely the code is not using batch statements, or is spreading data across multiple databases
and doesn't employ creative tricks. First, remedy the situation and introduce `batch` statements,
see {ref}`batch statements <batch-statements>`. Next, if needed, create a {ref}`discover-and-repair
script <discover-and-repair>`.

### Anti-pattern: Using full table scans in production code

Queries such as `select some_field from some_table;` are full table scans. Cassandra is not
optimized at all for such queries, and even with a small amount of data, a single such query can
completely mess up your whole cluster performance. We had an example of that which made our staging
environment unusable. Luckily, it was caught in time and
[fixed](https://github.com/wireapp/wire-server/pull/1574/files) before making its way to production.

Suggested alternative: Design your tables in a way to make use of a primary key, and always make use
of a `WHERE` clause: `SELECT some_field FROM some_table WHERE some_key = ?`.

In some rare circumstances you might not easily think of a good primary key. In this case, you could
for instance use a single default value that is hardcoded: `SELECT some_field FROM some_table WHERE
some_key = 1`. We use this strategy in the `meta` table which stores the cassandra version migration
information, and we use it for a 
[default idp](https://github.com/wireapp/wire-server/blob/4814afd88b8c832c4bd8c24674886c5d295aff78/services/spar/schema/src/V7.hs).
`some_field` might be of type `set`, which allows you to have some guarantees.
See the implementation of unique claims and the
[note on guarantees of CQL sets](https://github.com/wireapp/wire-server/blob/develop/services/brig/src/Brig/Unique.hs#L110)
for more information on sets.

### Anti-pattern: Using IN queries on a field in the partition key

Larger `IN` queries lead to performance problems. See [blog
post](https://lostechies.com/ryansvihla/2014/09/22/cassandra-query-patterns-not-using-the-in-query-for-multiple-partitions/)

A preferred way to do this lookup here is to use queries operating on single keys, and make
concurrent requests. In some cases `mapConcurrently` might be good, for large sets of data you may
wish to use the `pooledMapConcurrentlyN` function. To be conservative, you can use N=8 or N=32,
we've done this in other places and not seen problematic performance yet. Knowing what N should be
or if `mapConcurrently` is fine, we would ideally need load testing including performance metrics
(which we currently don't have - but which we should develop).

### Anti-pattern: Designing for a lot of deletes or updates

Cassandra works best for write-once read-many scenarios.

Read e.g.
- <https://www.datastax.com/blog/cassandra-anti-patterns-queues-and-queue-datasets>
- <https://www.instaclustr.com/support/documentation/cassandra/using-cassandra/managing-tombstones-in-cassandra/#>
- search the internet some more for 'cassandra' and 'tombstones' and if you find good posts, add them here.

## Understanding more about cassandra

### primary partition clustering keys

Confused about primary key, partition key, and clustering key? See e.g. [this post](https://blog.devgenius.io/cassandra-primary-vs-partitioning-vs-clustering-keys-3b3fa0e317f4) or [this one](https://dzone.com/articles/cassandra-data-modeling-primary-clustering-partiti)

### optimizing parallel request performance

See the thoughts in https://github.com/wireapp/wire-server/pull/1345#discussion_r567829234 - measuring overall and per-request performance and trying out different settings here might be worthwhile if increasing read or write performance is critical.

## Cassandra schema migrations

### Backwards compatible schema changes

Most cassandra schema changes are backwards compatible, or *should* be designed to be so. Looking at
the changes under `services/{brig,spar,galley,gundeck}/schema` you'll find this to be mostly the
case.

The general deployment setup for services interacting with cassandra have the following assumption:

* cassandra schema updates happen *before* new code is deployed.
  * This is safeguarded by the concourse deployment pipelines
  * This is also safeguarded by the `wire-server` helm chart, which deploys the `cassandra-migrations` job as part of a helm [pre-install/pre-upgrade hook](https://github.com/wireapp/wire-server/blob/b3b1af6757194aa1dc86a8f387887936f2afd2fb/charts/cassandra-migrations/templates/migrate-schema.yaml#L10-L13): that means the schema changes are applied, and helm waits before launching the new code in the brig/galley/spar/gundeck pods until the changes have completed applying.
  * This is further safeguarded by the code e.g. in brig refusing to even start the service up if the applied schema migration is not at least at a version the code expects it to. See [versionCheck](https://github.com/wireapp/wire-server/blob/b3b1af6757194aa1dc86a8f387887936f2afd2fb/services/brig/src/Brig/App.hs#L411) and [schemaVersion](https://github.com/wireapp/wire-server/blob/b3b1af6757194aa1dc86a8f387887936f2afd2fb/services/brig/src/Brig/App.hs#L136-L137)

So usually with these safeguards in place, and backwards-compatible changes, we have the following:

* At time t=0, old schema, old code serves traffic; all good.
* At time t=1, new schema, old code serves traffic: all good since backwards compatible.
* At time t=2, new schema, old code AND new code serve traffic: all good since backwards compatible.
* At time t=3, new schema, new code serves traffic: all good!

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
