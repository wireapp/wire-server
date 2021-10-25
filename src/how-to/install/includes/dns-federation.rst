DNS setup for federation
------------------------

SRV record
^^^^^^^^^^

One prerequisite to enable federation is an `SRV record <https://en.wikipedia.org/wiki/SRV_record>`__ as defined in `RFC
2782 <https://datatracker.ietf.org/doc/html/rfc2782>`__ that needs to be set up to allow the wire-server to be
discovered by other Wire backends. See the documentation on :ref:`discovery in federation<discovery>` for more
information on the role of discovery in federation.

The fields of the SRV record need to be populated as follows

* ``service``:  ``wire-server-federator``
* ``proto``: ``tcp``
* ``name``: <backend-domain>
* ``TTL``: e.g. 600 (10 minutes) in an initial phase. This can be set to a higher value (e.g. 86400) if your systems are stable and DNS records don't change a lot.
* ``priority``: anything. A good default value would be 0
* ``weight``: >0 for your server to be reachable. A good default value could be 10
* ``port``: ``443``
* ``target``: <federation-infra-domain>

To give an example, assuming

* your federation :ref:`Backend Domain <glossary_backend_domain>` is ``example.com``
* your domains for other services already set up follow the convention ``<service>.wire.example.org``

then your federation :ref:`Infra Domain <glossary_infra_domain>` would be ``federator.wire.example.org``.

The SRV record would look as follows:

.. code-block:: bash

   # _service._proto.name.                  ttl IN SRV priority weight port target.
   _wire-server-federator._tcp.example.com. 600 IN SRV 0        10     443  federator.wire.example.org.

DNS A record for the federator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Background: ``federator`` is the server component responsible for incoming and outgoing requests to other backend; but it is proxied on
the incoming requests by the ingress component on kubernetes as shown in :ref:`Federation Architecture<federation-architecture>`

As mentioned in :ref:`DNS setup for Helm<helmdns>`, you also need a ``federator.<domain>`` record, which, alongside your other DNS records that point to the ingress component, also needs to point to the IP of your ingress, i.e. the IP you want to provide services on.
