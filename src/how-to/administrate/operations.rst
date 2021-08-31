
Operational procedures
~~~~~~~~~~~~~~~~~~~~~~

This section describes common operations to be performed on operational clusters.

Reboot procedures
-----------------



Health checks
-------------



Check the health of a Cassandra node
....................................

To check the health of a Cassandra node, first log into the cassandra node:

.. code:: sh 

  ssh <ip of cassandra node>

Then run the following command: 

.. code:: sh 

  /opt/cassandra/bin/nodetool status

You should see a list of nodes like this:

.. code:: sh 

   Datacenter: datacenter1
   =======================
   Status=Up/Down
   |/ State=Normal/Leaving/Joining/Moving
   --  Address         Load       Tokens          Owns (effective)   Host ID                                Rack
   UN  192.168.220.13  9.51MiB    256             100.0%             3dba71c8-eea7-4e35-8f35-4386e7944894   rack1
   UN  192.168.220.23  9.53MiB    256             100.0%             3af56f1f-7685-4b5b-b73f-efdaa371e96e   rack1
   UN  192.168.220.33  9.55MiB    256             100.0%             RANDOMLY-MADE-UUID-GOES-INTHISPLACE!   rack1

A ``UN`` at the begginng of the line, refers to a node that is ``Up`` and ``Normal``.

Check the health of an ElastiSearch node
........................................

To check the health of an ElastiSearch node, first log into the elastisearch node:

.. code:: sh 

  ssh <ip of elastisearch node>

Then run the following command: 

.. code:: sh 

  curl localhost:9200/_cat/health

You should see output looking like this:

.. code:: 

  1630250355 15:18:55 elasticsearch-directory green 3 3 17 6 0 0 0 - 100.0%

Here, the ``green`` denotes good node health, and the ``3 3`` denotes 3 running nodes.
