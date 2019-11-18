General - Linux
--------------------------

.. include:: includes/intro.rst

Which ports and network interface is my process running on?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following shows open TCP ports, and the related processes.

.. code:: sh

   sudo netstat -antlp | grep LISTEN

which may yield output like this:

.. code:: sh

   tcp        0      0 0.0.0.0:22              0.0.0.0:*               LISTEN      1536/sshd
