# Connection {#RefConnection}

Two users can be _connected_ or not. If the users are connected, each of them can:

* Add the other user to a conversation (which is also a requirement for having a 1-1 conversation or doing a call).

* See the locale of the other user.

By default users with personal accounts are not connected. A user can send another user a _connection request_, which can be ignored or accepted by the other user. A user can also block an existing connection.

Members of the same team are always considered connected, see [Connections between team members](#RefConnectionTeam).

Internally, connection status is a _directed_ edge from one user to another that is attributed with a relation state and some meta information. If a user has a connection to another user, it can be in one of the six [connection states](#RefConnectionStates).

TODO describe autoconnection and onboarding.

## Connection states {#RefConnectionStates}

### Sent {#RefConnectionSent}

In order for two users to become connected, one of them performs a _connection request_ and the other one accepts it. Initiating a new connection results in a pending 1-1 conversation to be created with the sender as the sole member. When the connection is accepted, the other user joins the conversation.

The creator of a new connection (i.e. the sender of the connection request) ends up in this state. From the point of view of the creator, it indicates that a connection request has been sent but not accepted (it might be blocked or ignored).

### Pending {#RefConnectionPending}

The recipient of a connection request automatically ends up in this state.
From his point of view, the state indicates that the connection is pending
and awaiting further action (i.e. through accepting, ignoring or blocking it).

### Blocked {#RefConnectionBlocked}

When a connection is in this state it indicates that the user does not want to be bothered by the other user, e.g. by receiving messages, calls or being added to conversations.

Blocking a user does not prevent receiving further messages of that user in existing group conversations where the blocked user is a member.

When user A blocks user B, the connection restrictions apply to both users -- e.g. A can not add B to conversations, even though it's A who blocked B and not vice-versa.

### Ignored {#RefConnectionIgnored}

The recipient of a connection request may decide to explicitly "ignore" the request In this state the sender can continue to send further connection attempts. The recipient can change their mind and accept the request later.

### Cancelled {#RefConnectionCancelled}

This is a state that the sender can change to if the connection has not yet been accepted. The state will also change for the recipient, unless blocked.

### Accepted {#RefConnectionAccepted}

A connection in this state is fully accepted by a user. The user thus allows the user at the other end of the connection to add him to conversations.

For two users to be considered "connected", both A->B and B->A connections have to be in the "Accepted" state.

## Transitions between connection states {#RefConnectionTransitions}

![Connection state transitions](connection-transitions.png)

(To edit this diagram, open [connection-transitions.xml](connection-transitions.xml) with <https://draw.io>.)

## Connections between team members {#RefConnectionTeam}

Users belonging to the same team are always implicitly treated as connected, to make it easier for team members to see each other's profiles, create conversations, etc.

Since there is no explicit connection state between two team members, changing the connection status (e.g. blocking a fellow team member) is impossible.
