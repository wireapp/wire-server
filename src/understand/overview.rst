Overview
========

Introduction
------------

In a simplified way, the server components for wire involve the following:

|arch-simplified|

The Wire clients (such as the Wire app on your phone) connect either directly (or via a Load Balancer) to the "Wire Server". As "Wire Server" we mean multiple API server components that connect to each other, and which also connect to a few databases. Both the API components and the databases are each in a "cluster", which means copies of the same program code runs multiple times. This allows any one component to fail without users noticing that there is a problem (also called
"high-availability").

Architecture and networking
----------------------------

Note that the webapp, account pages, and team-settings, while in a way not part of backend,
are installed with the rest and therefor included.

Focus on internet protocols
~~~~~~~~~~~~~~~~~~~~~~~~~~~

|arch-proto|


Focus on high-availability
~~~~~~~~~~~~~~~~~~~~~~~~~~

The following diagram shows a usual setup with multiple VMs (Virtual Machines):

|arch-ha|

Wire clients (such as the Wire app on your phone) connect to a load balancer.

The Load balancer forwards traffic to the ingress inside the kubernetes VMs. (Restund is special, see :ref:`understand-restund` for details how Restund works)

The nginx ingress pods inside kubernetes look at incoming traffic, and forward that traffic on to the right place, depending on what's inside the URL passed. For example, if a request comes in for ``https://example-https.example.com``, it is forwarded to a component called ``nginz``, which is the main entry point for the `wire-server API <https://github.com/wireapp/wire-server>`__. If, however, a request comes in to ``https://webapp.example.com``, it is forwarded to a component called `webapp <https://github.com/wireapp/wire-webapp>`__, which hosts the graphical browser Wire client (as found when you open `<https://app.wire.com>`__.

Wire-server needs a range of databases. Their names are: cassandra, elasticsearch, minio, redis, etcd.

All the server components on one physical machine can connect to all the databases (also those on a different physical machine). The databases each connect to each-other, e.g. cassandra on machine 1 will connect to the cassandra VMs on machines 2 and 3.


.. |arch-simplified| image:: img/architecture-server-simplified.png
.. |arch-proto| image:: ./img/architecture-tls-on-prem-2020-09.png
.. |arch-ha| image:: ../how-to/install/img/architecture-server-ha.png
