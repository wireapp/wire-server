Implementation Roadmap
=======================

Internally at Wire, we have divided implemention of federation into multiple milestones. Only the milestone on which implementation has started will be shown here (as later milestones are subject to internal change and re-ordering)

M1 federation with proteus MVP on web
--------------------------------------

The first agreed milestone **M1** is a minimum-viable-product that allows users on different Wire backends to send textual messages to users on other backends.

M1 will have support for:

* user search, conversations, message sending, visual UX for showing federation

Caveats:

* Images, files, calling, etc will not yet be supported.
* Only the web client will be supported, mobile clients will not yet be supported.
* Federation with wire.com will not be supported. Instead, only on-premise installations of wire will have a way to enable federation via configuration flags.

M(N) | N >1
------------

* support more features (assets, calling, ...)
* support more platforms (android and iOS)
* support federating with wire.com
* support for better encryption primitives (MLS)


