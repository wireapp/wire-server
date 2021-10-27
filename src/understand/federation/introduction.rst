Introduction
============

Federation is a feature that allows a collection of Wire backends to enable the
establishment of connections among their respective users.

Goals
-----

If two Wire backends A and B are *federated*, the goal is for users of backend A
to be able to communicate with users of backend B and vice-versa in the same way
as if they were both part of the same backend.

Federated backends should be able to identify, discover and authenticate
one-another using the domain names under which they are reachable via the
network.

To enable federation, administrators of a Wire backend can decide to either
specifically list the backends that they want to federate with, or to allow federation with all Wire backends reachable from the network.

Federation is facilitated by two backend components: the *Federation Ingress*,
which, as the name suggests, acts as ingress point for federated traffic and the
*Federator*, which acts as egress point and processes all ingress requests from
the Federation Ingress after the authentication step.

Non-Goals
---------

We aim to integrate federation into the Wire backend following a step-by-step
process as described in the :ref:`federation roadmap<federation-roadmap>`. Early
versions are not meant to enable a completely open federation, but rather a
closed network of federated backends with a restricted set of features.

The aim of federation is not to replace the existing organizational structures
for Wire users such as teams and groups, but rather to complement them.
