Etcd
--------------------------

.. include:: includes/intro.rst

This section only covers the bare minimum, for more information, see the `etcd documentation <https://etcd.io/>`__

How to see cluster health
~~~~~~~~~~~~~~~~~~~~~~~~~~

If the file `/usr/local/bin/etcd-health.sh` is available, you can run

.. code:: sh

    etcd-health.sh

which should produce an output similar to::

    Cluster-Endpoints: https://127.0.0.1:2379
    cURL Command: curl -X GET https://127.0.0.1:2379/v2/members
    member 7c37f7dc10558fae is healthy: got healthy result from https://10.10.1.11:2379
    member cca4e6f315097b3b is healthy: got healthy result from https://10.10.1.10:2379
    member e767162297c84b1e is healthy: got healthy result from https://10.10.1.12:2379
    cluster is healthy

If that helper file is not available, create it with the following contents:

.. code:: bash

    #!/usr/bin/env bash

    HOST=$(hostname)

    etcdctl --endpoints https://127.0.0.1:2379 --ca-file=/etc/ssl/etcd/ssl/ca.pem --cert-file=/etc/ssl/etcd/ssl/member-$HOST.pem --key-file=/etc/ssl/etcd/ssl/member-$HOST-key.pem --debug cluster-health

and then make it executable: ``chmod +x /usr/local/bin/etcd-health.sh``

How to inspect tables and data manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: sh

    TODO


How to rolling-restart an etcd cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On each server one by one:

1. Check your cluster is healthy (see above)
2. Stop the process with ``systemctl stop etcd`` (this should be safe since etcd clients retry their operation if one endpoint becomes unavailable, see `this page <https://etcd.io/docs/v3.3.12/learning/client-architecture/>`__)
3. Do any operation you need, if any.
4. ``systemctl start etcd``
5. Wait for your cluster to be healthy again.
6. Do the same on the next server.


Troubleshooting
~~~~~~~~~~~~~~~~~~~~~~~~~~


How to recover from a single unhealthy etcd node after snapshot restore
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

After restoring an etcd machine from an earlier snapshot of the machine disk, etcd members may become unable to join.

Symptoms: That etcd process is unable to start and crashes, and other etcd nodes can't reach it::

    failed to check the health of member e767162297c84b1e on https://10.10.1.12:2379: Get https://10.10.1.12:2379/health: dial tcp 10.10.1.12:2379: getsockopt: connection refused
    member e767162297c84b1e is unreachable: [https://10.10.1.12:2379] are all unreachable

Logs from the crashing etcd::

    (...)
    Sep 25 09:27:05 node2 etcd[20288]: 2019-09-25 07:27:05.691409 I | raft: e767162297c84b1e [term: 28] received a MsgHeartbeat message with higher term from cca4e6f315097b3b [term: 30]
    Sep 25 09:27:05 node2 etcd[20288]: 2019-09-25 07:27:05.691620 I | raft: e767162297c84b1e became follower at term 30
    Sep 25 09:27:05 node2 etcd[20288]: 2019-09-25 07:27:05.692423 C | raft: tocommit(16152654) is out of range [lastIndex(16061986)]. Was the raft log corrupted, truncated, or lost?
    Sep 25 09:27:05 node2 etcd[20288]: panic: tocommit(16152654) is out of range [lastIndex(16061986)]. Was the raft log corrupted, truncated, or lost?
    Sep 25 09:27:05 node2 etcd[20288]: goroutine 90 [running]:
    (...)

To remediate the situation, do the following:

.. code:: sh

    TODO
