
Operational procedures
~~~~~~~~~~~~~~~~~~~~~~

This section describes common operations to be performed on operational clusters.

TOREMOVE: Julia Longtin
I would move the content into the sections (so people know how to health check), then move the "what to do about needing to reboot" stuff into a separate section, that says "perform health checks here, and here, and"...
10:12 AM
and probably add a word of caution that one needs to actually understand these instructions, they are not guaranteed correct for every environment, you can destroy your data, yadda yadda..
10:13 AM
"if you are unsure, contact your wire customer support representative", etc.

Reboot procedures
-----------------

The general procedure to reboot a pod is as follows:

* 1. `Check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the pod/service. (If the health isn't good, move to `troubleshooting <https://docs.wire.com/search.html?q=troubleshooting>`__. If it is good, move to the next step.)
* 2. Reboot the server the pod/service is running on.
* 3. `Check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the pod/service **again**. (If the health isn't good, move to `troubleshooting <https://docs.wire.com/search.html?q=troubleshooting>`__. If it is good, your reboot was succesful.)

The method for checking health is different for each pod/service type, you can find a list of those methods `here <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__.

The method to reset a pod is the same for most services, except for ``restund``, for which the procedure is different, and can be found `here <https://docs.wire.com/how-to/administrate/restund.html#rebooting-a-restund-node>`__.

For other (non-``restund``) pods, the procedure is as follows:

Assuming in this example you are trying to reboot a minio server, follow these steps:

First, `check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the pod.

Second, reboot the pod:

.. code:: sh 

  ssh -t <ip of minio node> sudo reboot

Third, `check the health <https://docs.wire.com/how-to/administrate/operations.html#health-checks>`__ of the pod again.

Health checks
-------------

This is a list of the health-checking procedures currently documented, for different pod/service types:

* `MinIO <https://docs.wire.com/how-to/administrate/minio.html#check-the-health-of-a-minio-node>`__.
* `Cassandra <https://docs.wire.com/how-to/administrate/cassandra.html#check-the-health-of-a-cassandra-node>`__.
* `ElastiSearch <https://docs.wire.com/how-to/administrate/elasticsearch.html#check-the-health-of-an-elastisearch-node>`__.
* `Restund <https://docs.wire.com/how-to/administrate/restund.html#rebooting-a-restund-node>`__ (the health check is explained as part of the reboot procedure).

To check the health of different services not listed here, see the documentation for that specific project, or ask your Wire contact.
