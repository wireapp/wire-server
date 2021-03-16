.. _understand-restund:

Restund (TURN) servers
========================

Introduction
~~~~~~~~~~~~

Restund servers allow two users on different networks (for
example Alice who is in an office connected to an office router and Bob
who is at home connected to a home router) to have a Wire audio or video
call. More precisely:

   Restund is a modular and flexible
   `STUN <https://en.wikipedia.org/wiki/STUN>`__ and
   `TURN <https://en.wikipedia.org/wiki/Traversal_Using_Relays_around_NAT>`__
   Server, with IPv4 and IPv6 support.

.. _architecture-restund:

Architecture
~~~~~~~~~~~~

Since the restund servers help establishing a connection between two
users, they need to be reachable by both of these users, which usually
means they need to have a **public IP address**.

While one server is enough to get started, two servers provide
high-availability in case one server gets into trouble.

You can either have restund servers directly exposed to the public
internet:

|architecture-restund|

Or you can have them reachable by fronting them with a firewall or load
balancer machine that may have a different IP than the server where
restund is installed:

|architecture-restund-lb|


Network
~~~~~~~

As briefly mentioned above, a TURN server functions as a bridge between
networks. Networks which don't have a direct route defined between them,
usually have distinct address blocks. Depending on the address block they
are configured with - such block is either considered to be *public* or *private*
(aka special-purpose addresses `[RFC 6890] <https://tools.ietf.org/html/rfc6890>`__)

- `IPv4 private blocks <https://www.iana.org/assignments/iana-ipv4-special-registry/iana-ipv4-special-registry.xhtml>`__
- `IPv6 private blocks <https://www.iana.org/assignments/iana-ipv6-special-registry/iana-ipv6-special-registry.xhtml>`__

In cases where a machine, that is hosting the TURN server, also connects
to a *private* network in which other services are running, chances are
that these services are being indirectly exposed through that TURN server.

To prevent this kind of exposure, a TURN server has to be configured with an inclusive
or exclusive list of address blocks to prevents undesired connections from being
established [1]_. At the moment (Feb. 2021), this functionality is not yet available
with *Restund* on the application-level. Instead, the system-level firewall capabilities
must be utilized. The `IP ranges <https://www.rtcsec.com/post/2021/01/details-about-cve-2020-26262-bypass-of-coturns-default-access-control-protection/#further-concerns-what-else>`__
mentioned in the article [1]_ should be blocked for egress and, depending on the scenario,
also for ingress traffic. Tools like ``iptables`` or ``ufw`` can be used to set this up.

.. [1] `Details about CVE-2020-26262, bypass of Coturn's default access control protection <https://www.rtcsec.com/post/2021/01/details-about-cve-2020-26262-bypass-of-coturns-default-access-control-protection/>`__


Protocols and open ports
~~~~~~~~~~~~~~~~~~~~~~~~

UDP
^^^

Restund servers provide the best audio/video connections if end-user
devices can connect to them via UDP. In this case, a firewall (if any)
needs to allow and/or forward the complete UDP port range ``1024-65535``
for incoming UDP traffic. Port ``3478`` is the default control port,
however one UDP port per active connection is required, so a whole port
range must be available and reachable from the outside.

In case e.g. office firewall rules disallow UDP traffic, there is a
possibility to use TCP instead, at the expense of call quality.

TCP
^^^

Two (configurable) ports are used by restund for TCP, one for plain TCP
and one for TLS. By default restund uses ports ``3478`` for plain TCP
and port ``5349`` for TLS. You can instead use (if that's easier with
firewall rules) for example ports ``80`` and ``443`` (requires to run
restund as root) or do a redirect from a load balancer (if using one) to
redirect ``443 -> 5349`` and ``80 -> 3478``.

Amount of users and file descriptors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each allocation (active connection by one participant) requires 1 or 2
file descriptors, so ensure you increase your file descriptor limits in
case you have many users.

Currently one restund server can have a maximum of 64000 allocations. If
you have more users than that in an active call, you need to deploy more
restund servers.

Load balancing and high-availability
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Load balancing is not possible, since STUN/TURN is a stateful protocol,
so UDP packets addressed to ``restund server 1``, if by means of a load
balancer were to end up at ``restund server 2``, would get dropped, as
the second server doesn't know the source address.

High-availability is nevertheless ensured by having and advertising more
than one restund server.  Instead of the load balancer, the clients will
switch their server if it fails.

Discovery and establishing a call
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A simplified flow of how restund servers, along with the wire-server are
used to establish a call:

|flow-restund|

DNS
~~~

Usually DNS records are used which point to the public IPs of the
restund servers (or of the respective firewall or load balancer
machines). These DNS names are then used when configuring wire-server.

.. |architecture-restund| image:: img/architecture-restund.png
.. |architecture-restund-lb| image:: img/architecture-restund-lb.png
.. |flow-restund| image:: img/flow-restund.png
