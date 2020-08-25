Restund (TURN)
--------------

.. include:: includes/intro.rst

.. _allocations:

How to see how many people are currently connected to the restund server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can see the count of currently ongoing calls (also called "allocations"):

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

1. Remove that restund server you want to restart from the list of advertised nodes, by taking it out of the turn server list that brig advertises:

Go to the place where you store kubernetes configuration for your wire-server installation. This might be a directory on your admin laptop, or a directory on the kubernetes machine.

If your override configuration (``values/wire-server/values.yaml``) looks like the following:

.. code:: yaml

  # (...)

  brig:
  # (...)
    turnStatic:
      v1:
        # v1 entries can be ignored and are not in use anymore since end of 2018.
      v2:
      - turn:server1.example.com:3478 # server 1 UDP
      - turn:server1.example.com:3478?transport=tcp # server 1 TCP
      - turns:server1.example.com:5478?transport=tcp # server 1 TLS
      - turn:server2.example.com:3478 # server 2 UDP
      - turn:server2.example.com:3478?transport=tcp # server 2 TCP
      - turns:server2.example.com:5478?transport=tcp # server 2 TLS

And you want to remove server 1, then change the configuration to read

.. code:: yaml

  turnStatic:
    v2:
      - turn:server2.example.com:3478 # server 2 UDP
      - turn:server2.example.com:3478?transport=tcp # server 2 TCP
      - turns:server2.example.com:5478?transport=tcp # server 2 TLS

(or comment out lines by adding a ``#`` in front of the respective line)

.. code:: yaml

    turnStatic:
      v2:
      #- turn:server1.example.com:3478 # server 1 UDP
      #- turn:server1.example.com:3478?transport=tcp # server 1 TCP
      #- turns:server1.example.com:5478?transport=tcp # server 1 TLS
      - turn:server2.example.com:3478 # server 2 UDP
      - turn:server2.example.com:3478?transport=tcp # server 2 TCP
      - turns:server2.example.com:5478?transport=tcp # server 2 TLS

Next, apply these changes to configuration with ``./bin/prod-setup.sh``

You then need to restart the ``brig`` pods if your code is older than September 2019 (otherwise brig will restart itself automatically):

.. code:: bash

  kubectl delete pod -l wireService=brig

2. Wait for traffic to drain. This can take up to 12 hours after the configuration change. Wait until current allocations (people connected to the restund server) return 0. See :ref:`allocations`.
3. It's now safe to ``systemctl stop restund``, and take any necessary actions.
4. ``systemctl start restund`` and then add the restund server back to configuration of advertised nodes (see step 1, put the server back).

How to renew a certificate for restund
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Replace the certificate file on the server (under ``/etc/restund/restund.pem`` usually), either with ansible or manually. Ensure the new certificate file is a concatenation of your whole certificate chain *and* the private key:

.. code:: text

  -----BEGIN CERTIFICATE-----
  ...
  -----END CERTIFICATE-----
  -----BEGIN CERTIFICATE-----
  ...
  -----END CERTIFICATE-----
  -----BEGIN PRIVATE KEY-----
  ...
  -----END PRIVATE KEY-----


2. Restart restund (see sections above)


How to check which restund/TURN servers will be used by clients
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The list of turn servers contacted by clients *should* match what you added to your `turnStatic` configuration. But if you'd like to double-check, here's how:

Terminal one:

.. code:: sh

   kubectl port-forward svc/brig 9999:8080

Terminal two:

.. code:: sh

   UUID=$(cat /proc/sys/kernel/random/uuid)
   curl -s -H "Z-User:$UUID" -H "Z-Connection:anything" "http://localhost:9999/calls/config/v2" | json_pp


May return something like:

.. code:: json

   {
      "ice_servers" : [
         {
            "credential" : "ASyFLXqbmg8fuK4chJG3S1Qg4L/nnhpkN0/UctdtTFbGW1AcuuAaOqUMDhm9V2w7zKHY6PPMqjhwKZ2neSE78g==",
            "urls" : [
               "turn:turn1.example.com:3478"
            ],
            "username" : "d=1582157904.v=1.k=0.t=s.r=mbzovplogqxbasbf"
         },
         {
            "credential" : "ZsxEtGWbpUZ3QWxPZtbX6g53HXu6PWfhhUfGNqRBJjrsly5w9IPAsuAWLEOP7fsoSXF13mgSPROXxMYAB/fQ6g==",
            "urls" : [
               "turn:turn1.example.com:3478?transport=tcp"
            ],
            "username" : "d=1582157904.v=1.k=0.t=s.r=jsafnwtgqhfqjvco"
         },
         {
            "credential" : "ZsxEtGWbpUZ3QWxPZtbX6g53HXu6PWfhhUfGNqRBJjrsly5w9IPAsuAWLEOP7fsoSXF13mgSPROXxMYAB/fQ6g==",
            "urls" : [
               "turns:turn1.example.com:5349?transport=tcp"
            ],
            "username" : "d=1582157904.v=1.k=0.t=s.r=jsafnwtgqhfqjvco"
         }
      ],
      "ttl" : 3600
   }

In the above case, there is a single server configured to use UDP on port 3478, plain TCP on port 3478, and TLS over TCP on port 5349. The ordering of the list is random and will change on every request made with curl.
