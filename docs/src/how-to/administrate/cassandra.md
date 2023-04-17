# Cassandra

```{eval-rst}
.. include:: includes/intro.rst
```

This section only covers the bare minimum, for more information, see the [cassandra
documentation](https://cassandra.apache.org/doc/latest/)

(check-the-health-of-a-cassandra-node)=

## Check the health of a Cassandra node

To check the health of a Cassandra node, run the following command:

```sh
ssh <ip of cassandra node> /opt/cassandra/bin/nodetool status
```

or if you are running a newer version of wire-server (altough it should be backwards compatibile)

```sh
ssh <ip of cassandra node> /opt/cassandra/bin/nodetool -h ::FFFF:127.0.0.1 status
```

You should see a list of nodes like this:

```sh
Datacenter: datacenter1
=======================
Status=Up/Down
|/ State=Normal/Leaving/Joining/Moving
--  Address         Load       Tokens          Owns (effective)   Host ID                                Rack
UN  192.168.220.13  9.51MiB    256             100.0%             3dba71c8-eea7-4e35-8f35-4386e7944894   rack1
UN  192.168.220.23  9.53MiB    256             100.0%             3af56f1f-7685-4b5b-b73f-efdaa371e96e   rack1
UN  192.168.220.33  9.55MiB    256             100.0%             RANDOMLY-MADE-UUID-GOES-INTHISPLACE!   rack1
```

A `UN` at the begginng of the line, refers to a node that is `Up` and `Normal`.

You can also check the logs of the cassandra server with 

```
journalctl -u cassandra.service 
```

## How to inspect tables and data manually

```sh
cqlsh
# from the cqlsh shell
describe keyspaces
use <keyspace>;
describe tables;
select * from <tablename> WHERE <primarykey>=<some-value> LIMIT 10;
```

If your local install does not have cqlsh available, you can use docker instead:

```
sudo docker run -it --rm cassandra:3.11 cqlsh 172.16.0.132 9042
```

## How to rolling-restart a cassandra cluster

For maintenance you may need to restart the cluster.

On each server one by one:

1. check your cluster is healthy: `nodetool status` or `nodetool -h ::FFFF:127.0.0.1 status` (in newer versions)
2. `nodetool drain && systemctl stop cassandra` (to stop accepting writes and flush data to disk; then stop the process)
3. do any operation you need, if any
4. Start the cassandra daemon process: `systemctl start cassandra`
5. Wait for your cluster to be healthy again.
6. Do the same on the next server.
