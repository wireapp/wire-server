module Galley.Federation
  ( -- * translating server-server API response to Galley types
    mapConversationUpdateResult,
    mapConversationEvent,
    qualifyConversationUpdateResult,
    UpdateResult (..),
  )
where

import Data.Id (makeMappedIdOpaque)
import Data.IdMapping (IdMapping (_imMappedId))
import Galley.API.IdMapping (createConvIdMapping, createUserIdMapping)
import Galley.App (Galley)
import Galley.Types (Event (Event), EventData (EdMembersJoin), EventType (MemberJoin), SimpleMember (SimpleMember), SimpleMembers (SimpleMembers))
import Galley.Types.Conversations.Roles (RoleName, roleNameWireAdmin, roleNameWireMember)
import Imports
import qualified Wire.API.Federation.API.Conversation as Fed
import qualified Wire.API.Federation.Event as Fed

-- TODO(federation): move to galley-types?
data UpdateResult
  = Updated Event
  | Unchanged

-- qualified -> unqualified

mapConversationUpdateResult ::
  Functor f =>
  (Fed.ConversationEvent a -> f Event) ->
  Fed.ConversationUpdateResult a ->
  f UpdateResult
mapConversationUpdateResult f = \case
  Fed.ConversationUnchanged -> pure Unchanged
  Fed.ConversationUpdated ev -> Updated <$> f ev

-- | We need to generalize this at some point
mapConversationEventMemberJoin :: Fed.ConversationEvent Fed.MemberJoin -> Galley Event
mapConversationEventMemberJoin ev =
  Event
    <$> pure MemberJoin
    <*> (makeMappedIdOpaque . _imMappedId <$> createConvIdMapping (Fed.eventConversation ev))
    <*> (makeMappedIdOpaque . _imMappedId <$> createUserIdMapping (Fed.eventFrom ev))
    <*> pure (Fed.eventTime ev)
    <*> mapMemberJoin (Fed.eventData ev)

mapMemberJoin :: Fed.MemberJoin -> Galley EventData
mapMemberJoin (Fed.MemberJoin simpleMembers) =
  EdMembersJoin . SimpleMembers <$> traverse mapSimpleMember simpleMembers

mapSimpleMember :: Fed.SimpleMember -> Galley SimpleMember
mapSimpleMember m =
  SimpleMember
    <$> (makeMappedIdOpaque . _imMappedId <$> createUserIdMapping (Fed.smId m))
    <*> pure (mapConversationRole (Fed.smConversationRole m))

mapConversationRole :: Fed.ConversationRole -> RoleName
mapConversationRole = \case
  Fed.ConversationRoleAdmin -> roleNameWireAdmin
  Fed.ConversationRoleMember -> roleNameWireMember
