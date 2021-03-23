.. _install-sft:

Installing Conference Calling 2.0 (aka SFT)
===========================================

Background
~~~~~~~~~~

Please refer to the following :ref:`section to better understand SFT and how it works <understand-sft>`.


As part of the wire-server umbrella chart
-----------------------------------------

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
----------

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


.. _install-sft-firewall-rules:

Firewall rules
--------------

The SFT allocates media addresses in the ``32768-61000`` UDP range. Ingress and
egress traffic should be allowed for this range. Furthermore the SFT needs to be
able to reach the :ref:`Restund server <understand-restund>`, as it uses STUN and TURN in cases the client
can not directly connect to the SFT. In practise this means the SFT should
allow ingress and egress traffic on the UDP port range ``32768-61000`` from and
to both clients and the :ref:`Restund server <understand-restund>`.

The SFT also has an HTTP interface for initializing (allocation) or joining (signaling) a call. This is exposed through
the ingress controller as an HTTPS service.

An SFT instance does **not** communicate with other SFT instances.

*For more information, please refer to the source code of the Ansible role:* `sft-server <https://github.com/wireapp/ansible-sft/blob/develop/roles/sft-server/tasks/traffic.yml>`__.
