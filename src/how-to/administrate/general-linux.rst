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

How can I see if my TLS certificates are configured the way I expect?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use openssl to check, with e.g.

.. code:: sh

   DOMAIN=example.com
   PORT=443
   echo Q | openssl s_client -showcerts -connect $DOMAIN:$PORT

or

.. code:: sh

   DOMAIN=example.com
   PORT=443
   echo Q | openssl s_client -showcerts -connect $DOMAIN:$PORT 2>/dev/null | openssl x509 -inform pem -noout -text

To see only the validity (expiration):

.. code:: sh

   DOMAIN=example.com
   PORT=443
   echo Q | openssl s_client -showcerts -connect $DOMAIN:$PORT 2>/dev/null | openssl x509 -inform pem -noout -text | grep Validity -A 2
