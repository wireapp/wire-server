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

Rebooting a Restund node
~~~~~~~~~~~~~~~~~~~~~~~~

If you want to reboot a restund node, you need to make sure the other restund nodes in the cluster are running, so that services are not interrupted by the reboot.

Presuming your two restund nodes are called:

* ``restund-1``
* ``restund-2``

To prepare for a reboot of ``restund-1``, log into the other restund server (``restund-2``, for example here), and make sure the docker service is running.

List the running containers, to ensure restund is running, by executing:

.. code:: sh

  ssh -t <ip of restund-2> sudo docker container ls

You should see the following in the results:

.. code:: sh

  CONTAINER ID         IMAGE                                COMMAND         STATUS        PORTS    NAMES
  <random hash>        quay.io/wire/restund:v0.4.16b1.0.53  22 seconds ago  Up 18 seconds          restund

Make sure you see this restund container, and it is running ("Up"). 

If it is not, you need to do troubleshooting work, if it is running, you can move forward and reboot restund-1.

Now log into the restund server you wish to reboot (``restund-1`` in this example), and reboot it

.. code:: sh
  
  ssh -t <ip of restund-1> sudo reboot

Wait at least a minute for the machine to restart, you can use this command to automatically retry SSH access until it is succesful:

.. code:: sh 

  ssh -o 'ConnectionAttempts 3600' <ip of restund-1 node> exit

Then log into the restund server (``restund-1``, in this example), and make sure the docker service is running:

.. code:: sh

  ssh -t <ip of restund-1> sudo docker container ls

.. code:: sh

  CONTAINER ID         IMAGE                                COMMAND         STATUS        PORTS    NAMES
  <random hash>        quay.io/wire/restund:v0.4.16b1.0.53  22 seconds ago  Up 18 seconds          restund

Here again, make sure you see a restund container, and it is running ("Up").

If it is, you have succesfully reboot the restund server, and can if you need to apply the same procedure to the other restund servers in your cluster.