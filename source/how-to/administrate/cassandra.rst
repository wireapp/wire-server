Cassandra
--------------------------

.. include:: includes/intro.rst

This section only covers the bare minimum, for more information, see the `cassandra
documentation <https://cassandra.apache.org/doc/latest/>`__

How to see cluster health
~~~~~~~~~~~~~~~~~~~~~~~~~~

You want to see `UN` (`Up` & `Normal`).

::

   nodetool status

How to inspect tables and data manually
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   cqlsh
   # from the cqlsh shell
   describe keyspaces
   use <keyspace>;
   describe tables;
   select * from <tablename> WHERE <primarykey>=<some-value> LIMIT 10;

How to restart cassandra
~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO


