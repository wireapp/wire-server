Minio
------

.. include:: includes/intro.rst

This section only covers the bare minimum, for more information, see the `minio documentation <https://docs.min.io/>`__

Initialize interaction with minio::

   # from a minio machine

   mc config host add server1 http://localhost:9000 <access_key> <access_secret>

   mc admin info server1

