Restund (TURN)
--------------

.. include:: includes/intro.rst

How to see how many people are currently connected to the restund server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can see the amount of ongoing calls:

.. code:: sh

   echo turnstats | nc -u 127.0.0.1 33000 -q1 | grep allocs_cur | cut -d' ' -f2

How to restart restund (with downtime)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Please note that restarting ``restund`` means any user that is
currently connected to it (i.e. having a call) will lose its audio/video
connection. If you wish to have no downtime, check the next section*

With downtime, it's very easy::

   systemctl restart restund

How to restart restund without having downtime
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For maintenance you may need to restart a restund server.

1. Remove that restund server you want to restart from the list of advertised nodes (by taking it out of the turn server list that brig advertises, see the `turnStatic configuration <https://github.com/wireapp/wire-server-deploy/blob/master/charts/brig/values.yaml#L68-L73>`_
2. Wait for traffic to drain. This can take up to 12 hours after the configuration change. Wait until current allocations (people connected to the restund server) return 0.
3. It's now safe to ``systemctl restart restund``
4. Add the restund server back to configuration of advertised nodes.

How to renew a certificate for restund
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Replace the certificate file on the server
2. Restart restund (see sections above)
