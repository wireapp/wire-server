.. _tls:

Configure TLS ciphers
=======================

The following table lists recommended ciphers for TLS server setups, which should be used in wire deployments.


============================= ======= ============ ================= ========================
Cipher                        Version Wire default `BSI TR-02102-2`_ `Mozilla TLS Guideline`_
============================= ======= ============ ================= ========================
ECDHE-ECDSA-AES128-GCM-SHA256 TLSv1.2 no           **yes**           intermediate
ECDHE-RSA-AES128-GCM-SHA256   TLSv1.2 no           **yes**           intermediate
ECDHE-ECDSA-AES256-GCM-SHA384 TLSv1.2 **yes**      **yes**           intermediate
ECDHE-RSA-AES256-GCM-SHA384   TLSv1.2 **yes**      **yes**           intermediate
ECDHE-ECDSA-CHACHA20-POLY1305 TLSv1.2 no           no                intermediate
ECDHE-RSA-CHACHA20-POLY1305   TLSv1.2 no           no                intermediate
TLS_AES_128_GCM_SHA256        TLSv1.3 **yes**      **yes**           **modern**
TLS_AES_256_GCM_SHA384        TLSv1.3 **yes**      **yes**           **modern**
TLS_CHACHA20_POLY1305_SHA256  TLSv1.3 no           no                **modern**
============================= ======= ============ ================= ========================


.. _bsi tr-02102-2: https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.pdf
.. _mozilla tls guideline: https://wiki.mozilla.org/Security/Server_Side_TLS

.. note::
   If you enable TLSv1.3, openssl does always enable the three default cipher suites for TLSv1.3.
   Therefore it is not necessary to add them to openssl based configurations.

.. _ingress traffic:

Ingress Traffic (wire-server)
-----------------------------
The list of TLS ciphers for incoming requests is limited by default to the `following <https://github.com/wireapp/wire-server/blob/master/charts/nginx-ingress-controller/values.yaml#L7>`_ (for general server-certificates, both for federation and client API), and can be overridden on your installation if needed.


Egress Traffic (wire-server/federation)
---------------------------------------
The list of TLS ciphers for outgoing federation requests is currently hardcoded, the list is `here <https://github.com/wireapp/wire-server/blob/master/services/federator/src/Federator/Remote.hs#L164-L180>`_.


SFTD (ansible)
--------------
The list of TLS ciphers for incoming SFT requests (and metrics) are defined in ansible templates `sftd.vhost.conf.j2 <https://github.com/wireapp/ansible-sft/blob/develop/roles/sft-server/templates/sftd.vhost.conf.j2#L19>`_ and `metrics.vhost.conf.j2 <https://github.com/wireapp/ansible-sft/blob/develop/roles/sft-server/templates/metrics.vhost.conf.j2#L13>`_.

SFTD (kubernetes)
-----------------
SFTD deployed via kubernetes uses ``kubernetes.io/ingress`` for ingress traffic, configured in `ingress.yaml <https://github.com/wireapp/wire-server/blob/develop/charts/sftd/templates/ingress.yaml>`_.
Kubernetes based deployments make use of the settings from :ref:`ingress traffic`.


Restund (ansible)
-----------------

The list of TLS ciphers for "TLS over TCP" TURN (and metrics) are defined in ansible templates `nginx-stream.conf.j2 <https://github.com/wireapp/ansible-restund/blob/master/templates/nginx-stream.conf.j2#L25>`_ and `nginx-metrics.conf.j2 <https://github.com/wireapp/ansible-restund/blob/master/templates/nginx-metrics.conf.j2#L15>`_.

Restund (kubernetes)
--------------------
`Kubernetes restund <https://github.com/wireapp/wire-server/tree/develop/charts/restund>`_ deployment does not provide TLS connectivity.
