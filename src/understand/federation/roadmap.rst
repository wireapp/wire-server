.. _federation-roadmap:

Implementation Roadmap
=======================

Internally at Wire, we have divided implemention of federation into multiple milestones. Only the milestone on which implementation has started will be shown here (as later milestones are subject to internal change and re-ordering)

M1 federation with proteus MVP
------------------------------

The first milestone **M1** is a minimum-viable-product that allows users on different Wire backends to send textual messages to users on other backends.

M1 included support for:

* user search
* creating group conversations
* message sending
* visual UX for showing federation.
* a way for on-premise (self-hosted) installations of wire to try out this implementation of federation by explicitly enabling it via configuration flags.
* Android, Web and iOS will be supported
* server2server discovery and authentication
* a way to specify an allow list of backends to federate with


M2 federation with calling/conferencing and assets
--------------------------------------------------

The second milestone **M2** focused on:

* federated calling
* federated conferencing
* basic federated asset support. 

**M2** also incorporated a previous interim release which added the following in a federated environment:

* likes
* mentions
* read receipts and delivery notifications
* pings
* edit and delete messages

Caveats:

* Message delivery guarantees are weak if any backends are temporarily unavailable.
* If any backends are unavailable, data inconsistencies may occur.
* Federation with the production cloud version of wire.com is not yet supported.
* Federated conferencing requires an SFT in each domain represented in the conversation. The caller's SFT is the "anchor" SFT, to which federated SFTs connect:

  * SFTs must have valid certificates suitable for mutual authentication with federated SFTs. 
  * Currently all video streams are exchanged between the anchor SFT and each federated SFT. The SFTs select the relevant streams for each client as today, but inter-SFT traffic could use substantially more bandwidth than an SFT to client stream.
  * The administrator needs to open ports between their SFTs and federated SFTs for signalling and media.
* Assets will be stored on the backend of the sender and fetched via the sender's backend with every access (there is no caching on a federated domain). If federated domains have different policies for allowed asset types or sizes, a user may receive notification of an asset which it is not allowed to fetch or view.

.. note::
   A rough (Backend) Implementation Status as of January 2022:

   Tested in M2 scope:
     * Federator as Egress, and Ingress support to allow backend-backend communication
     * Long-running test environments
     * Backend Discovery via SRV records
     * Backend allow list support
     * User search via exact handle
     * Get user profile, user clients, and prekeys for their clients
     * Create conversation with remote users
     * Send a message in a conversation with remote users
     * Server2server authentication
     * connections
     * Assets
     * Calling 
     * Conferencing

   Partially done:
     * client-server API changes for federation
     * Other conversation features (removing users, archived/muted, ...)

Additional Milestones
---------------------

Some additional milestones planned include the following features:

* support more features (guest users, bots, ...)
* support better message delivery guarantees
* federation API versioning strategy
* support for wire-server installations to federate with wire.com
* MLS support
