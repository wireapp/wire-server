
Installing Conference Calling 2.0 (aka SFT)
======================================================

Background
------------

Previously, Wire group calls were implemented as a mesh, where each participant was connected
to each other in a peer-to-peer fashion. This meant that a client would have to upload their
video and audio feeds separately for each participant. This in practice meant that the amount
of participants was limited by the upload bandwidth of the clients.


Wire now has a signalling-forwarding unit called `SFT <https://github.com/wireapp/wire-avs-service>`_ which allows clients to upload once and
then the SFT fans it out to the other clients. Because connections are not end-to-end anymore now, dTLS encryption offered by WebRTC is not enough anymore as the encryption is terminated at the server-side. To avoid Wire from seeing the contents of calls SFT utilises WebRTC InsertibleStreams to encrypt the packets a second time with a group key that is not known to the server.

Using SFT large but secure audio and video calls are possible.



Prerequisites
-------------

The SFT needs to be reachable by clients on its public IP and should be able to reach the TURN servers as well for it to function.

Only one SFT instance can run per node so if you want to scale up your SFT instance you will need to increase the amount of kubernetes nodes in your cluster.

As a rule of thumb you will need 1vCPU of compute per 50 participants.

For more information about capacity planning and networking please refer to the `technical documentation <https://github.com/wireapp/wire-server/blob/eab0ce1ff335889bc5a187c51872dfd0e78cc22b/charts/sftd/README.md>`_


Deploying the SFT
------------------

The SFT component is shipped as a separate helm chart. Installation is similar to installing
the charts as in :ref:`helm_prod`.

By default ``sftd`` doesn't need to set that many options, so we define them inline. However, you could of course also set these values in a ``values.yaml`` file.

SFT will deploy a Kubernetes Ingress on ``$SFTD_HOST``.  Make sure that the domain name ``$SFTD_HOST`` points to your ingress IP as set up in :ref:`helm_prod`.  The SFT also needs to be made aware of the domain name of the webapp that you set up in :ref:`helm_prod` for setting up the appropriate CSP headers.

.. code:: shell

   export SFTD_HOST=sftd.example.com
   export WEBAPP_HOST=webapp.example.com

Now you can install the chart:

.. code:: shell

    helm upgrade --install sftd wire/sftd --set
    helm install sftd wire/sftd  \
      --set host=$SFTD_HOST \
      --set allowOrigin=https://$WEBAPP_HOST \
      --set-file tls.crt=/path/to/tls.crt \
      --set-file tls.key=/path/to/tls.k

For more advanced setups please refer to the `technical documentation <https://github.com/wireapp/wire-server/blob/eab0ce1ff335889bc5a187c51872dfd0e78cc22b/charts/sftd/README.md>`_

Configuring wire-server
-----------------------

You should make sure that in ``values/wire-server/values.yaml`` you set  ``brig.optSettings.setSftStaticUrl`` to ``https://$SFTD_HOST:443`` where ``$SFTD_HOST`` is replaced with the domain name of the SFT server. Then roll out the change:


.. code:: shell

   helm upgrade wire-server wire/wire-server --values ./values/wire-server/values.yaml

