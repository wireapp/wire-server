

-- NOTE: we should model all services in one effect tree, and then see which interface parts
-- can be put into a separate service.  the separation between brig and galley has stopped
-- making sense with the introduction of teams.

data ConversationConfig = ConversationConfig {
    defaultRole :: 
    canHaveGuests ::
    canHaveBots ::
    readReceipts :: 
    ...
  }

data Permission a
  = CanCreate a
  | CanUpdate a
  | CanDelete a
  | CanGet a

data Conversation

data Member -- member is user plus extra metadata, also there is team and conv member.

data Invitation -- this will become a member

data ConversationRole

-- A type family on effects, ie `ConversationEffect AccessRules` etc?
data AccessRulesConversationEffect where
  CanCreate :: UserId -> ConversationId -> ... -> Maybe CanCreateWitness
  CanDelete :: .. -> Maybe CanDeleteWitness
  -- CanUpdate ::

-- CRUD on conversation requres:
-- user id, which maps to role, which maps to permissions
-- mapping from role to permission is static, role -> [permission]
-- mapping user to role, dynamic, part of the user account information
-- With that, we can from a user id and a conversation able to map to 
-- access rules for that conversation.

-- Can we use access rules effects to create witnesses to authorisations?
data ConversationEffect m a where
  Create :: CanCreateWitness -> ConversationConfig -> m Result -- (containing ConvId)
  Delete :: CanDeleteWitness -> ConvId -> m ()
  UpdateConfig :: ...Witness -> ConvId -> ConversationConfig -> m Result
  AddMembers :: ...Witness -> ConvId -> [Invitation] -> m Result
  RemoveMembers :: ...Witness -> ConvId -> [MemberId] -> m Result
  ChangeRole :: ...Witness -> ConvId -> MemberId -> Role -> m Result
  -- TODO: deal with messages inside a conversation?

data Witness a = Witness

data AccessRulesNotificationEffect where
  CanSendNotification :: ConversationId -> Notification -> Maybe (Witness Send)

data NotificationEffect m a where
  Send :: Witness Send -> Notification -> m NotificationId
  FetchNewNotifications :: Witness Fetch -> NotificationFilter -> m [Notification]
  OpenNotificationStream :: NotificationFilter -> m Stream
  CloseNotificationStream :: Stream -> m ()

data Notification = { conversationId, notificationId, payload, notiftype, expires, ... }
data NotificationFilter = { userId, clientId, conversationId, timeRange, lastSeenNotificationId }

-- data MLSNonsense m a where


data UserAccount m a where
  -- crud + validation + authorization/authentication

  createUserAccount ::
  -- team-owner-email
  -- team-member-invitation
  -- team-member-scim
  -- team-member-saml (deprecated)
  -- personal-user-email ?
  -- personal-user-phone ?
  --
  -- auth:
  --   saml
  --   oauth (sci-fi)
  --   password/zauth
  --
  -- teamrole:
  --   owner, admin, member, external, ...
  --
  -- TODO: how do email and saml differ categorically?  (identity vs. auth mechanism)

  readUserAccount ::
  -- by userid
  -- by email
  -- by handle
  -- by ...

  updateUserAccount ::
  -- same keys as read, but with a delta to the UserAccount record as extra argument
  -- alternative: have many actions that update individual aspects

  deleteUserAccount ::
  -- same keys as read
  
  --
  authoriseUserAccount :: 
  -- no, better construct a context that is required by the application
  -- logic for authorization here.
  authenticateUserAccount :: Credentials -> m Bool 

data TeamEffect m a where

data TeamRole

data Team -- a collection of users, where a user belongs to a single team, 1 : 1

data TeamConfig

data Device
