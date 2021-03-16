Installing Conference Calling 2.0 (aka SFT)
===========================================

Background
----------

Previously, Wire group calls were implemented as a mesh, where each participant was connected
to each other in a peer-to-peer fashion. This meant that a client would have to upload their
video and audio feeds separately for each participant. This in practice meant that the amount
of participants was limited by the upload bandwidth of the clients.

Wire now has a signalling-forwarding unit called `SFT <https://github.com/wireapp/wire-avs-service>`__ which allows clients to upload once and
then the SFT fans it out to the other clients. Because connections are not end-to-end anymore now, dTLS encryption offered by WebRTC is not enough anymore as the encryption is terminated at the server-side. To avoid Wire from seeing the contents of calls SFT utilises WebRTC InsertibleStreams to encrypt the packets a second time with a group key that is not known to the server.

With SFT it is thus possible to have conference calls with many participants
without compromising end-to-end security.


Prerequisites
-------------

The SFT needs to be reachable by clients on its public IP and should be able to reach the TURN servers as well for it to function.

The SFT needs to be able to receive and send traffic over UDP and TCP on a wide range of ports.
Due to the fact that Kubernetes services do not support setting port ranges, and kubernetes pods not being publicly routable (at least in IPv4) we require the SFT pods to run in `hostNetwork` mode and the pod will bind directly to the default interface of the node.

Due to this `hostNetwork` limitation only one SFT instance can run per node so if you want to scale up your SFT deployment you will need to increase the amount of kubernetes nodes in your cluster.

As a rule of thumb you will need 1vCPU of compute per 50 participants. SFT will utilise multiple cores. You can use this rule of thumb to decide how many kubernetes nodes you need to provision.

For more information about capacity planning and networking please refer to the `technical documentation <https://github.com/wireapp/wire-server/blob/eab0ce1ff335889bc5a187c51872dfd0e78cc22b/charts/sftd/README.md>`__


Deploying the SFT
-----------------

As part of the wire-server umbrella chart
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, ``sftd`` will be installed as part of the ``wire-server`` umbrella chart.

In your ``./values/wire-server/values.yaml`` file you should set the following settings:

.. code:: yaml

   sftd:
     host: sftd.example.com # Replace example.com with your domain
     allowOrigin: webapp.example.com # Should be the address you used for the webapp deployment

In your ``secrets.yaml`` you should set the TLS keys for sftd domain:

.. code:: yaml

   sftd:
     tls:
       crt: |
         <TLS CRT HERE>
       key: |
         <TLS KEY HERE>
          
You should also make sure that you configure brig to know about the SFT server in your ``./values/wire-server/values.yaml``  file:

.. code:: yaml
   
   brig:
     optSettings:
       setSftStaticUrl: "https://sftd.example.com:443"

Now you can deploy as usual:

.. code:: shell

   helm upgrade wire-server wire/wire-server --values ./values/wire-server/values.yaml

          
Standalone
^^^^^^^^^^

The SFT component is also shipped as a separate helm chart. Installation is similar to installing
the charts as in :ref:`helm_prod`.

Some people might want to run SFT separately, because the deployment lifecycle for the SFT is a bit more intricate. For example,
if you want to avoid dropping calls during an upgrade, you'd set the ``terminationGracePeriodSeconds`` of the SFT to a high number, to wait
for calls to drain before updating to the new version (See  `technical documentation <https://github.com/wireapp/wire-server/blob/develop/charts/sftd/README.md>`__).  that would cause your otherwise snappy upgrade of the ``wire-server`` chart to now take a long time, as it waits for all
the SFT servers to drain. If this is a concern for you, we advice installing ``sftd`` as a separate chart.

It is important that you disable ``sftd`` in the ``wire-server`` umbrella chart, by setting this in your ``./values/wire-server/values.yaml``  file

.. code:: yaml

   tags:
     sftd: false
      

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
      --set-file tls.key=/path/to/tls.key
      
You should also make sure that you configure brig to know about the SFT server, in the ``./values/wire-server/values.yaml`` file:

.. code:: yaml
   
   brig:
     optSettings:
       setSftStaticUrl: "https://sftd.example.com:443"

And then roll-out the change to the ``wire-server`` chart

.. code:: shell

   helm upgrade wire-server wire/wire-server --values ./values/wire-server/values.yaml

For more advanced setups please refer to the `technical documentation <https://github.com/wireapp/wire-server/blob/develop/charts/sftd/README.md>`__.
