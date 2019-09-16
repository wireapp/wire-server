Etcd
--------------------------

.. include:: includes/intro.rst

This section only covers the bare minimum, for more information, see the `etcd documentation <https://etcd.io/>`__

How to see cluster health
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: sh

   TODO

How to inspect tables and data manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: sh

    TODO


How to rolling-restart an etcd cluster
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On each server one by one:

1. check your cluster is healthy (see above)
2. 


3. ``systemctl stop etcd`` (to stop the process)
4. do any operation you need, if any
5. ``systemctl start etcd``
6. Wait for your cluster to be healthy again.
7. Do the same on the next server.


