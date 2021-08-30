
Operations procedures
~~~~~~~~~~~~~~~~~~~~~

This section describes common operations performed on operational clusters.


Reboot procedures
-----------------

If you want to reboot a restund node, you need to make sure the other restund nodes in the cluster are running, so that services are not interrupted by the reboot.

Presuming your two restund nodes are called:

* restund-1
* restund-2

To prepare for a reboot of restund-1, log into the other restund server (restund-2, for example here), and make sure the docker service is running.

List the running containers, to ensure restund is running, by executing:

.. code:: sh

  ssh -t <ip of restund-2> sudo docker container ls

You should see the following in the results:

.. code:: sh

  CONTAINER ID         IMAGE                                COMMAND         STATUS        PORTS    NAMES
  <random hash>        quay.io/wire/restund:v0.4.16b1.0.53  22 seconds ago  Up 18 seconds          restund

Make sure you see this restund container, and it is running ("Up"). 

If it is not, you need to do troubleshooting work, if it is running, you can move forward and reboot restund-1.

Now log into the restund server you wish to reboot (restund-1 in this example), and reboot it

.. code:: sh
  
  ssh -t <ip of restund-1> sudo reboot

Wait at least a minute for the machine to restart, then log into the restund server (restund-1, in this example), and make sure the docker service is running:

.. code:: sh

  ssh -t <ip of restund-1> sudo docker container ls

.. code:: sh

  CONTAINER ID         IMAGE                                COMMAND         STATUS        PORTS    NAMES
  <random hash>        quay.io/wire/restund:v0.4.16b1.0.53  22 seconds ago  Up 18 seconds          restund

Here again, make sure you see a restund container, and it is running ("Up").

If it is, you have succesfully reboot the restund server, and can if you need to apply the same procedure to the other restund servers in your cluster.

Health checks
-------------

Check the health of a MinIO node
................................

This is the procedure to check a minio node's health.

First log into the minio server 

.. code:: sh 

  ssh <ip of minio node>

There, run the following commands:

.. code:: sh

  env $(sudo grep KEY /etc/default/minio-server1 | xargs) bash
  export MC_HOST_local="http://$MINIO_ACCESS_KEY:$MINIO_SECRET_KEY@127.0.0.1:9000"
  mc admin info local

You should see a result similar to this:

.. code:: sh

   *  192.168.0.12:9092
   Uptime: 2 months
   Version: 2020-10-28T08:16:50Z
   Network: 6/6 OK
   Drives: 1/1 OK

   *  192.168.0.22:9000
   Uptime: 2 months
   Version: 2020-10-28T08:16:50Z
   Network: 6/6 OK
   Drives: 1/1 OK

   *  192.168.0.22:9092
   Uptime: 2 months
   Version: 2020-10-28T08:16:50Z
   Network: 6/6 OK
   Drives: 1/1 OK

   *  192.168.0.32:9000
   Uptime: 2 months
   Version: 2020-10-28T08:16:50Z
   Network: 6/6 OK
   Drives: 1/1 OK

   *  192.168.0.32:9092
   Uptime: 2 months
   Version: 2020-10-28T08:16:50Z
   Network: 6/6 OK
   Drives: 1/1 OK

   *  192.168.0.12:9000
   Uptime: 2 months
   Version: 2020-10-28T08:16:50Z
   Network: 6/6 OK
   Drives: 1/1 OK

Make sure you see ``Network: 6/6 OK``.

Reboot the machine with:

.. code:: sh 

  sudo reboot 

Then wait at least a minute.

If you go to ssh in, and get 'Connection refused', it just means you need to wait a bit longer.

Log into minio ( repeat the steps above ), and check again.

You should see a very low uptime value on two hosts now.

This is because we install minio 'twice' on each host.

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
