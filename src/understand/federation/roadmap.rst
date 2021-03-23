Implementation Roadmap
=======================

Internally at Wire, we have divided implemention of federation into multiple milestones. Only the milestone on which implementation has started will be shown here (as later milestones are subject to internal change and re-ordering)

M1 federation with proteus MVP on web
--------------------------------------

The first agreed milestone **M1** is a minimum-viable-product that allows users on different Wire backends to send textual messages to users on other backends.

M1 will have support for:

* user search, conversations, message sending, visual UX for showing federation.
* a way for on-premise (self-hosted) installations of wire to try out this implementation of federation by explicitly enabling it via configuration flags.

Caveats:

* Message delivery guarantees will be weak if any backends are temporarily unavailable.
* Images, files, calling, etc will not yet be supported.
* Only the web client will be supported, mobile clients will not yet be supported.
* Federation with the production cloud version of wire.com will not yet be supported.

.. note::
   Status March 2021:
     * implementation: under active development

M(N) | N >1
------------

.. note::
   Status March 2021:
     * planning & design phase: ongoing
     * implementation: not started yet

* support more features (assets, calling, ...)
* support better message delivery guarantees
* support more platforms (android and iOS)
* support for wire-server installations to federate with wire.com
* support for better encryption primitives (MLS)


