.. _understand-sft:

Conference Calling 2.0 (aka SFT)
================================

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


Architecture
------------

The following diagram is centered around SFT and its role within a calling setup. Restund is seen
as a mere client proxy and its relation to and interaction with a client is explained
:ref:`here <understand-restund>`. The diagram shows that a call resides on a single SFT instance
and that the instance allocates at least one port for media transport per participant in the call.

.. figure:: img/architecture-sft.png

    SFT signaling, and media sending from the perspective of one caller


Establishing a call
-------------------

1. *Client A* wants to initiate a call. It contacts all the known SFT servers via HTTPS.
   The SFT server that is quickest to respond is the one that will be used by the client.
   (Request 1: ``CONFCONN``)
2. *Client A* gathers connection candidates (own public IP, public IP of the network the
   client is in with the help of STUN, through TURN servers) [1]_ for the SFT server to
   establish a media connection to *Client A*. These information are then being send again
   from *Client A* to the chosen SFT server via HTTPS request. (Request 2: ``SETUP``)
3. The SFT server tests which of the connection candidates actually work. Meaning, it
   goes through all the candidates until one leads to a successful media connection
   between itself and *client A*
4. *Client A* sends an OTR [2]_ message ``CONFSTART`` (to all members of chat), which contains
   the IP address of the SFT server that is being used for the call.
5. Any other client that wants to join the call, does 1. + 2. with the exception of **only**
   contacting one SFT server i.e. the one that *client A* chose and told all other
   potential participants about via ``CONFSTART`` message

At that point a media connection between *client A* and the SFT server has been established,
and they continue talking to each other by using the data-channel, which uses the media
connection (i.e. no more HTTPS at that point). There are just 2 HTTPS request/response
sequences per participant.

.. [1] STUN & TURN are both part of a :ref:`Restund server <understand-restund>`
.. [2] Off The Record - an encrypted message sent in a conversation hidden from user's view but
       interpreted by user's clients. It is sent via backend servers and forwarded to other
       conversation participants, not to or via SFT.


Prerequisites
-------------

For Conference Calling to function properly, clients need to be able to reach the HTTPS interface
of the SFT server(s) - either directly or through a load balancer sitting in front of the servers.
This is only needed for the call initiation/joining part.
Additionally, for the media connection, clients and SFT servers should be able to reach each other
via UDP (see :ref:`Firewall rules <install-sft-firewall-rules>`).
If that is not possible, then at least SFT servers and Restund servers should be able to reach each
other via UDP - and clients may connect via UDP and/or TCP to Restund servers
(see :ref:`Protocols and open ports <understand-restund-protocal-and-ports>`), which in
turn will connect to SFT server.
In the unlikely scenario where no UDP is allowed whatsoever or SFT servers may not be able to reach
the Restund servers that clients are using to make themselves reachable, an SFT server itself can
also choose to proxy itself by a Restund server, which could be different from the Restund servers
used by clients (see *TURN discovery* flag).

The SFT may need to receive and send traffic over UDP and TCP on a wide range of ports.
Due to the fact that Kubernetes services do not support setting port ranges, and Kubernetes pods not being publicly routable (at least in IPv4) we require the SFT pods to run in `hostNetwork` mode and the pod will bind directly to the default interface of the node.

Due to this `hostNetwork` limitation only one SFT instance can run per node so if you want to scale up your SFT deployment you will need to increase the amount of kubernetes nodes in your cluster.

As a rule of thumb you will need 1vCPU of compute per 50 participants. SFT will utilise multiple cores. You can use this rule of thumb to decide how many kubernetes nodes you need to provision.

For more information about capacity planning and networking please refer to the `technical documentation <https://github.com/wireapp/wire-server/blob/eab0ce1ff335889bc5a187c51872dfd0e78cc22b/charts/sftd/README.md>`__
