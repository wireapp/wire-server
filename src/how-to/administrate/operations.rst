
Operational procedures
~~~~~~~~~~~~~~~~~~~~~~

This section describes common operations to be performed on operational clusters.

Reboot procedures
-----------------

The general procedure to reboot a service is as follows:

* 1. `Check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the service. (If the health isn't good, move to `troubleshooting <https://docs.wire.com/search.html?q=troubleshooting>`__. If it is good, move to the next step.)
* 2. Reboot the server the service is running on.
* 3. `Check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the service **again**. (If the health isn't good, move to `troubleshooting <https://docs.wire.com/search.html?q=troubleshooting>`__. If it is good, your reboot was succesful.)

The method for checking health is different for each service type, you can find a list of those methods `here <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__.

The method to reset a service is the same for most services, except for ``restund``, for which the procedure is different, and can be found `here <https://docs.wire.com/how-to/administrate/restund.html#rebooting-a-restund-node>`__.

For other (non-``restund``) services, the procedure is as follows:

Assuming in this example you are trying to reboot a minio server, follow these steps:

First, `check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the services.

Second, reboot the services:

.. code:: sh 

  ssh -t <ip of minio node> sudo reboot

Third, wait until the service is up again by trying to connect to it via SSH :

.. code:: sh 

  ssh -o 'ConnectionAttempts 3600' <ip of minio node> exit

(``ConnectionAttempts`` will make it so it attempts to connect until the host is actually Up and the connection is succesful)

Fourth, `check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the service again.

Health checks
-------------

This is a list of the health-checking procedures currently documented, for different service types:

* `MinIO <https://docs.wire.com/how-to/administrate/minio.html#check-the-health-of-a-minio-node>`__.
* `Cassandra <https://docs.wire.com/how-to/administrate/cassandra.html#check-the-health-of-a-cassandra-node>`__.
* `ElastiSearch <https://docs.wire.com/how-to/administrate/elasticsearch.html#check-the-health-of-an-elastisearch-node>`__.
* `Etcd <https://docs.wire.com/how-to/administrate/etcd.html#how-to-see-cluster-health>`__.
* `Restund <https://docs.wire.com/how-to/administrate/restund.html#rebooting-a-restund-node>`__ (the health check is explained as part of the reboot procedure).

To check the health of different services not listed here, see the documentation for that specific project, or ask your Wire contact.

Draining pods from a node for maintainance
------------------------------------------

You might want to remove («drain») all pods from a specific node/server, so you can do maintainance work on it, without disrupting the entire cluster.

If you want to do this, you should follow the procudure found at: https://kubernetes.io/docs/tasks/administer-cluster/safely-drain-node/

In short, the procedure is essentially:

First, identify the name of the node you wish to drain. You can list all of the nodes in your cluster with

.. code:: sh 

  kubectl get nodes

Next, tell Kubernetes to drain the node:

.. code:: sh 

  kubectl drain <node name>

Once it returns (without giving an error), you can power down the node (or equivalently, if on a cloud platform, delete the virtual machine backing the node). If you leave the node in the cluster during the maintenance operation, you need to run

.. code:: sh 

  kubectl uncordon <node name>

afterwards to tell Kubernetes that it can resume scheduling new pods onto the node.

