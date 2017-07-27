
## User Connections

### Concepts

Connections between users are a means for giving users some control over their
privacy and a degree of spam protection. Connections control whether a user can:

  * Add another user to a conversation.
  * See the full profile (including email address and phone number) of another user.

Thus, implicitly through conversation membership, the connections control whether a user can:

  * Post to a 1-1 conversation with another user.
  * Establish a call with another user.

In order for two users to become connected, one of them performs a connection request
and the other one accepts it. Initiating a new connection results in a pending 1-1
conversation to be created with the sender as the sole member. When the connection is
accepted, the other user joins the conversation.

Blocking a user does not prevent receiving further messages of that user in existing
group conversations where the blocked user is a member.

In the implementation, a connection is a directed edge from one user to another that is
attributed with a relation state and potentially other meta information.

### Connection States

The following are the existing states that connections can be in and their meaning.

#### Sent

The creator of a new connection (i.e. the sender of the connection request) ends
up in this state. From the point of view of the creator, it indicates that a
connection request has been sent but not accepted (it might be blocked or ignored).

#### Pending

The recipient of a connection request automatically ends up in this state.
From his point of view, the state indicates that the connection is pending
and awaiting further action (i.e. through accepting, ignoring or blocking it).

#### Blocked

When a connection is in this state it indicates that the user does not want
to be bothered by the other user, e.g. by receiving messages, calls or being added
to conversations.

#### Ignored

This is a temporary state for the recipient of a connection request. It indicates
that the recipient neither wanted to accept nor block the connection (yet). In this state
the sender can continue to send further connection attempts.

#### Cancelled

This is a state that the sender can change to if the connection has not
yet been accepted. The state will also change for the recipient, unless
blocked.

#### Accepted

A connection in this state is fully accepted by a user. The user thus allows the
user at the other end of the connection to add him to conversations, view his full
profile information, etc.


### Transitions

The following diagram depicts the transitions between connection states.

![Connection State Transitions](connections.png)

In order to edit this diagram, open the [connections.xml](connections.xml)
with [draw.io](https://www.draw.io/).
