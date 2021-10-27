.. _federation-roadmap:

Implementation Roadmap
=======================

Internally at Wire, we have divided implemention of federation into multiple milestones. Only the milestone on which implementation has started will be shown here (as later milestones are subject to internal change and re-ordering)

M1 federation with proteus MVP
------------------------------

The first agreed milestone **M1** is a minimum-viable-product that allows users on different Wire backends to send textual messages to users on other backends.

M1 will have support for:

* user search
* creating group conversations
* message sending
* visual UX for showing federation.
* a way for on-premise (self-hosted) installations of wire to try out this implementation of federation by explicitly enabling it via configuration flags.
* Android, Web and iOS will be supported
* server2server discovery and authentication
* a way to specify an allow list of backends to federate with

Caveats:

* Message delivery guarantees will be weak if any backends are temporarily unavailable.
* If any backends are unavailable, data inconsistencies may occur.
* Images, files, calling, etc will not yet be supported.
* Edit, delete, delivery notifications, mentions, replies, etc will not yet be supported. 
* Federation with the production cloud version of wire.com will not yet be supported.

.. note::
   A rough (Backend) Implementation Status as of October 2021:

   Completed for M1 scope:
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

   Partially done:
     * client-server API changes for federation
     * Other conversation features (removing users, archived/muted, ...)

Additional Milestones
---------------------

Some additional milestones planned include the following features:

* conversation feature completeness
* support more features (assets, calling, ...)
* support better message delivery guarantees
* federation API versioning strategy
* support for wire-server installations to federate with wire.com
* MLS support
