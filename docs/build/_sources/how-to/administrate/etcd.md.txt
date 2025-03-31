# Etcd

```{eval-rst}
.. include:: includes/intro.rst
```

This section only covers the bare minimum, for more information, see the [etcd documentation](https://etcd.io/)

(how-to-see-cluster-health)=

## How to see cluster health

If the file `/usr/local/bin/etcd-health.sh` is available, you can run

```sh
etcd-health.sh
```

which should produce an output similar to:

```
Cluster-Endpoints: https://127.0.0.1:2379
cURL Command: curl -X GET https://127.0.0.1:2379/v2/members
member 7c37f7dc10558fae is healthy: got healthy result from https://10.10.1.11:2379
member cca4e6f315097b3b is healthy: got healthy result from https://10.10.1.10:2379
member e767162297c84b1e is healthy: got healthy result from https://10.10.1.12:2379
cluster is healthy
```

If that helper file is not available, create it with the following contents:

```bash
#!/usr/bin/env bash

HOST=$(hostname)

etcdctl --endpoints https://127.0.0.1:2379 --ca-file=/etc/ssl/etcd/ssl/ca.pem --cert-file=/etc/ssl/etcd/ssl/member-$HOST.pem --key-file=/etc/ssl/etcd/ssl/member-$HOST-key.pem --debug cluster-health
```

and then make it executable: `chmod +x /usr/local/bin/etcd-health.sh`

## How to inspect tables and data manually

```sh
TODO
```

(how-to-rolling-restart-an-etcd-cluster)=

## How to rolling-restart an etcd cluster

Etcd is a consistent and partition tolerant key-value store.  This means that
Etcd nodes can be restarted (one by one) with no impact to the consistency of
data, but there might a small time in which the database can not process
writes. Etcd has a designated leader which decides ordering of events (and thus
writes) in the cluster. When the leader crashes, a leadership election takes
place.  During the leadership election, the cluster might be briefly
unavailable for writes.  Writes during this period are queued up until a new
leader is elected.  Any writes that were happening during the crash of the
leader that were not acknowledged by the leader and the followers yet will be
'lost'.  The client that performed this write will experience this as a write
timeout. (Source: <https://etcd.io/docs/v3.4.0/op-guide/failures/>).  Client
applications (like kubernetes) are expected to deal with this failure scenario
gracefully.

Etcd can be restarted in a rolling fashion, by cleanly shutting down and
starting up  etcd servers one by one.  In Etcd 3.1 and up, when the leader is
cleanly shut down, it will hand over leadership gracefully to another node,
which will minimize the impact of write-availability as election time is
reduced. (Source :
<https://kubernetes.io/blog/2018/12/11/etcd-current-status-and-future-roadmap/>)
Restarting follower nodes has no impact to availability.

Etcd does load-balancing between servrvers on the client-side. This means that
if a server you were talking to is being restarted, etcd will transparently
redirect the request to another server. It's is thus safe to shut them down at
any point.

Now to perform a rolling restart of the cluster, do the following steps:

1. Check your cluster is healthy (see above)
2. Stop the process with `systemctl stop etcd` (this should be safe since etcd clients retry their operation if one endpoint becomes unavailable, see [this page](https://etcd.io/docs/v3.3.12/learning/client-architecture/))
3. Do any operation you need, if any. Like rebooting
4. `systemctl start etcd`
5. Wait for your cluster to be healthy again.
6. Do the same on the next server.

*For more details please refer to the official documentation:* [Replacing a failed etcd member](https://kubernetes.io/docs/tasks/administer-cluster/configure-upgrade-etcd/#replacing-a-failed-etcd-member)

(etcd-backup-and-restore)=

## Backing up and restoring

Though as long as quorum is maintained in etcd there will be no dataloss, it is
still good to prepare for the worst. If a disaster takes out too many nodes, then
you might have to restore from an old backup.

Luckily, etcd can take periodic snapshots of your cluster and these can be used
in cases of disaster recovery. Information about how to do snapshots and
restores can be found here:
<https://github.com/etcd-io/etcd/blob/master/Documentation/op-guide/recovery.md>

*For more details please refer to the official documentation:* [Backing up an etcd cluster](https://kubernetes.io/docs/tasks/administer-cluster/configure-upgrade-etcd/#backing-up-an-etcd-cluster)

## Troubleshooting

### How to recover from a single unhealthy etcd node after virtual machine snapshot restore

After restoring an etcd machine from an earlier snapshot of the machine disk, etcd members may become unable to join.

Symptoms: That etcd process is unable to start and crashes, and other etcd nodes can't reach it:

```
failed to check the health of member e767162297c84b1e on https://10.10.1.12:2379: Get https://10.10.1.12:2379/health: dial tcp 10.10.1.12:2379: getsockopt: connection refused
member e767162297c84b1e is unreachable: [https://10.10.1.12:2379] are all unreachable
```

Logs from the crashing etcd:

```
(...)
Sep 25 09:27:05 node2 etcd[20288]: 2019-09-25 07:27:05.691409 I | raft: e767162297c84b1e [term: 28] received a MsgHeartbeat message with higher term from cca4e6f315097b3b [term: 30]
Sep 25 09:27:05 node2 etcd[20288]: 2019-09-25 07:27:05.691620 I | raft: e767162297c84b1e became follower at term 30
Sep 25 09:27:05 node2 etcd[20288]: 2019-09-25 07:27:05.692423 C | raft: tocommit(16152654) is out of range [lastIndex(16061986)]. Was the raft log corrupted, truncated, or lost?
Sep 25 09:27:05 node2 etcd[20288]: panic: tocommit(16152654) is out of range [lastIndex(16061986)]. Was the raft log corrupted, truncated, or lost?
Sep 25 09:27:05 node2 etcd[20288]: goroutine 90 [running]:
(...)
```

Etcd will refuse nodes that run behind to join the cluster.  If a node has
committed to a certain version of the raft log, it is expected not to jump back
in time after that. In this scenario, we turned an etcd server off, made a
snapshot of the virtual machine, brought it back online, and then restored the
snapshot.  What went wrong is is that if you bring up a VM snapshot, it means
the etcd node will now have an older raft log than it had before; even though
it already gossiped to all other nodes that it has knowledge of newer entries.

As a safety precaution, the other nodes will reject the node that is travelling
back in time, to avoid data corruption. A node could get corrupted for other
reasons as well. Perhaps a disk is faulty and is serving wrong data. Either
way, if you end up in a scenario where a node is unhealthy and will refuse to
rejoin the cluster, it is time to do some operations to get the cluster back in
a healthy state.

It is not recommended to restore an etcd node from a vm snapshot, as that will
cause these kind of time-travelling behaviours which will make the node
unhealthy. To recover from this situation anyway,
I quote from the etcdv2 admin guide <https://github.com/etcd-io/etcd/blob/master/Documentation/v2/admin_guide.md>

> If a member’s data directory is ever lost or corrupted then the user should
> remove the etcd member from the cluster using etcdctl tool.  A user should
> avoid restarting an etcd member with a data directory from an out-of-date
> backup. Using an out-of-date data directory can lead to inconsistency as the
> member had agreed to store information via raft then re-joins saying it
> needs that information again. For maximum safety, if an etcd member suffers
> any sort of data corruption or loss, it must be removed from the cluster.
> Once removed the member can be re-added with an empty data directory.

Note that this piece of documentation is from etcdv2 and not etcdv3. However
the etcdv3 docs describe a similar procedure here
<https://github.com/etcd-io/etcd/blob/master/Documentation/op-guide/runtime-configuration.md#replace-a-failed-machine>

The procedure to remove and add a member is documented here:
<https://github.com/etcd-io/etcd/blob/master/Documentation/op-guide/runtime-configuration.md#remove-a-member>

It is also documented in the kubernetes documentation:
<https://kubernetes.io/docs/tasks/administer-cluster/configure-upgrade-etcd/#replacing-a-failed-etcd-member>

So following the above guides step by step, we can recover our cluster to be
healthy again.

First let us make sure our broken member is stopped by runnning this on `node`:

```sh
systemctl stop etcd
```

Now from a healthy node, e.g. `node0` remove the broken node

```sh
etcdctl3.sh  member remove e767162297c84b1e
```

And we expect the output to be something like

```sh
Member e767162297c84b1e removed from cluster 432c10551aa096af
```

By removing the member from the cluster, you signal the other nodes to not
expect it to come back with the right state. It will be considered dead and
removed from the peers.  This will allow the node to come up with an empty data
directory and it not getting kicked out of the cluster. The cluster should now
be healthy, but only have 2 members, and so it is not to resistent to crashes
at the moment! As we can see if we run the health check from a healthy node.

```sh
etcd-health.sh
```

And we expect only two nodes to be in the cluster:

```
Cluster-Endpoints: https://127.0.0.1:2379
cURL Command: curl -X GET https://127.0.0.1:2379/v2/members
member 7c37f7dc10558fae is healthy: got healthy result from https://10.10.1.11:2379
member cca4e6f315097b3b is healthy: got healthy result from https://10.10.1.10:2379
cluster is healthy
```

Now from a healthy node, re-add the node you just removed. Make sure
to replace the IP in the snippet below with the IP of the node you just removed.

```sh
etcdctl3.sh member add etcd_2 --peer-urls https://10.10.1.12:2380
```

And it should report that it has been added:

```
Member e13b1d076b2f9344 added to cluster 432c10551aa096af

ETCD_NAME="etcd_2"
ETCD_INITIAL_CLUSTER="etcd_1=https://10.10.1.11:2380,etcd_0=https://10.10.1.10:2380,etcd_2=https://10.10.1.12:2380"
ETCD_INITIAL_CLUSTER_STATE="existing"
```

it should now be in the list as "unstarted"  instead of it not being in the list at all.

```sh
etcdctl3.sh member list


7c37f7dc10558fae, started, etcd_1, https://10.10.1.11:2380, https://10.10.1.11:2379
cca4e6f315097b3b, started, etcd_0, https://10.10.1.10:2380, https://10.10.1.10:2379
e13b1d076b2f9344, unstarted, , https://10.10.1.12:2380,
```

Now on the broken node, remove the on-disk state, which was corrupted, and start etcd

```sh
mv /var/lib/etcd /var/lib/etcd.bak
sudo systemctl start etcd
```

If we run the health check now, the cluster should report its healthy now again.

```sh
etcd-health.sh
```

And indeed it outputs so:

```
Cluster-Endpoints: https://127.0.0.1:2379
cURL Command: curl -X GET https://127.0.0.1:2379/v2/members
member 7c37f7dc10558fae is healthy: got healthy result from https://10.10.1.11:2379
member cca4e6f315097b3b is healthy: got healthy result from https://10.10.1.10:2379
member e13b1d076b2f9344 is healthy: got healthy result from https://10.10.1.12:2379
cluster is healthy
```
