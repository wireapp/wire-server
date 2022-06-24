Wire aims to support the [MLS
protocol](https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html).
Since MLS doesn't provide any concrete specs for Delivery Service and
Authentication Service this leaves many design choices for the integration with
Wire.

The work-in-progress API design handles MLS Commit Messages directly: clients
don't make Wire-specific requests to the backend, that are out-of-band to the
MLS protocol. For example for a Commit that contains an Add Proposal the backend
inspects the Commit message, authorizes it (checks if the user is allowed to add
to the conversation), updates its state (Wire user gets added to the Wire
conversation) then distributes the Commit message to all clients in the Group.
This design however requires that all Commit messages are sent in MLSPlaintext.

The purpose of this document is to explore an alternate API design that supports
Commits sent as a MLSCiphertext message, i.e the backend can only inspect its
envelope but not its contents. Within this this design all actions such as
adding and removing clients from Groups require clients to make additional
Wire-specific requests in addition to sending Commit message, Proposals etc. We
want to analyse consequences for security and robustness.

In this document we refer to a "client" as device that communicates with Wire
and also acts as client in the MLS protocol.


Requirements for Wire's backend:

1. The backend must forward all MLS messages to all the clients in the Group.

2. The backend must authorize Commits. E.g. before forwarding a Commit with an
   Add proposal the backend checks that the sender's user is an admin of the
   underlying conversation.
   
3. The backend must be able to execute Commits. When committing a Add / Remove
   proposal then the backend must start / stop forwarding Group messages to
   the respective clients.
    
4. The backend must arbitrate concurrently arriving Commit messages.


The proposed API design follows these principles:

- MLS messages are considered mostly opaque to the backend. This is certainly
  true for MLSCiphertext messages and for simplicity the backend will ignore the
  contents of MLSPlaintext messages too.

- All endpoints require that all Wire-specific information is bundled together
  in the same request. Compared to a design of having 2 request (1 for the mls
  message + 1 for wire-specific) this has the advantage of being more atomic and
  less error prone, e.g clients might fail to send the second request.

Example: Adding a client to an existing group (Commit with an Add proposal)

```
POST /mls/commit
Authorization: ...
{
  "proposals_wire": [
    {
      "proposal_type": "Add",
      "proposal_data": {
         "conversation": {
           "domain": "example.com",
           "id": "71dae459-27d8-43c8-a4e0-2eee941160d1",
         },
         "add_user": {
           "domain": "example.com",
           "id": "99db9768-04e3-4b5d-9268-831b6a25c4ab",
         },
         "client": "BiXXIPEu...",
         "conversation_role": "wire_member"
      }
    }
  ]
  "commit": "bGv6zUPfUl2o..." /* tls+base64 encoded */
}
```

The backend performs authorization of the Commit based on:
- checking that sender in the envelope corresponds to the user authenticated by token
- checking if the user has admin permissions for the conversation

The backend/client contract requires that the Wire-specific information in
`proposals_wire` corresponds to the Proposals that are part of the Commit in
field `commit`. The backend has no way of verifying this. However, if considered
separately guarantees hold: Changes to Wire's state based on the
`proposals_wire` information are correct regardless of the MLS state. Changes to
the MLS are correct regardless of Wire's backend state.

If clients don't honour this contract, two cases of inconsistency that might arise:

1. A user is in the a Wire conversation, but their client is not part of the
   Group (i.e. none of its Key Packages are in the ratchet tree) that is
   associated with the conversation. In this case no client will not be able to
   decrypt any of MLSCipherText messages forwarded by the backend. It will
   receive all information that is not encrypted: envelope data and MLSPlainText
   messages. The user will also receive information though Wire that every user
   of conversation of conversation does, e.g. events (user joins, ...)
   
2. A user's client is part of the Group but their user is not in the
   corresponding Wire conversation. In this case this client will not receive
   any MLS messages, because the backend won't forward them.


<!-- To meet the requirements the backend maintains a mapping from MLS entities to -->
<!-- Wire entities: -->

<!-- | MLS           | Wire                      | -->
<!-- |---------------|---------------------------| -->
<!-- | Group         | qualified Conversation    | -->
<!-- | KeyPackageRef | qualified User + clientId | -->
