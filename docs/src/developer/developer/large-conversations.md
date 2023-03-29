# Refactoring galley to support large conversations

To be able to suppport large conversations galley needs refactoring. This
section in the developer docs is meant to contain useful references for this
endeavor.

## Call hierarchy

See [large-conversations.yaml](https://github.com/wireapp/wire-server/blob/develop/docs/src/developer/developer/large-conversations.yaml) for 
 call hierarchy of functions that load the full member list into memory.

## Galley Endpoints

These are all the endpoints in galley with response types that contain the full member list of conversations.

In the `ConversationAPI`:

For type `Conversation`:
- `get-unqualified-conversation`
- `get-unqualified-conversation-legalhold-alias`
- `get-conversation@v2`
- `get-conversation`
- `get-mls-self-conversation`

For type `ConversationList Conversation`
- `get-conversations`

For type `ConversationsResponse`:
- `list-conversations@v1`
- `list-conversations@v2`
- `list-conversations`

For type `ConversationResponse`:
- `create-group-conversation@v2`
- `create-group-conversation@v3`
- `create-one-to-one-conversation@v2`
- `create-one-to-one-conversation`

For type `CreateGroupConversationResponse`:
- `create-group-conversation`
- `create-self-conversation@v2`
- `create-self-conversation@v2`
- `create-self-conversation`

For type `Wire.API.Event.Conversation.Event` (`EventData` might contain a `Conversation` object, but it's not clear from API type alone if it contains one)
- `add-members-to-conversation-unqualified`
- `add-members-to-conversation-unqualified2`
- `add-members-to-conversation`
- `join-conversation-by-id-unqualified`
- `join-conversation-by-code-unqualified`
- `create-conversation-code-unqualified`
- `remove-code-unqualified`
- `remove-member-unqualified`
- `update-conversation-name-deprecated`
- `update-conversation-name-unqualified`
- `update-conversation-name`
- `update-conversation-message-timer-unqualified`
- `update-conversation-message-timer`
- `update-conversation-receipt-mode`
- `update-conversation-access-unqualified`
- `update-conversation-access@v2`
- `update-conversation-access`

In the `MLSAPI`:

- `mls-commit-bundle` returns a `MLSMessageSendingStatus` which contains list of `Event`, as well a list of unreachable users.
- `mls-message-v1` and `mls-message` returns list of `Event`

The `MessagingAPI` (Proetus) is not listed here, assuming that Proteus conversations won't support large conversations.
