module Galley.Federation
  ( -- * translating server-server API response to Galley types
    mapConversationUpdateResult,
    mapEvent,
    UpdateResult (..),
  )
where

import Data.Id (makeMappedIdOpaque)
import Data.IdMapping (IdMapping (idMappingLocal))
import Galley.API.Util (createConvIdMapping, createUserIdMapping)
import Galley.App (Galley)
import Galley.Types (Event (Event), EventData (EdMembersJoin), EventType (MemberJoin), SimpleMember (SimpleMember), SimpleMembers (SimpleMembers))
import Galley.Types.Conversations.Roles (RoleName, roleNameWireAdmin, roleNameWireMember)
import Imports
import qualified Wire.API.Federation.Conversation as Fed
import qualified Wire.API.Federation.Types.Event as Fed

-- TODO(federation): move to galley-types?
data UpdateResult
  = Updated Event
  | Unchanged

mapConversationUpdateResult :: Fed.ConversationUpdateResult Fed.AnyEventData -> Galley UpdateResult
mapConversationUpdateResult = \case
  Fed.ConversationUnchanged -> pure Unchanged
  Fed.ConversationUpdated ev -> Updated <$> mapEvent ev

mapEvent :: Fed.Event Fed.AnyEventData -> Galley Event
mapEvent ev =
  Event
    <$> pure (toEventType (Fed.eventData ev))
    <*> (makeMappedIdOpaque . idMappingLocal <$> createConvIdMapping (Fed.eventConversation ev))
    <*> (makeMappedIdOpaque . idMappingLocal <$> createUserIdMapping (Fed.eventFrom ev))
    <*> pure (Fed.eventTime ev)
    <*> mapEventData (Fed.eventData ev)
  where
    toEventType = \case
      Fed.DataMemberJoin _ -> MemberJoin

mapEventData :: Fed.AnyEventData -> Galley (Maybe EventData)
mapEventData = \case
  Fed.DataMemberJoin (Fed.MemberJoin simpleMembers) ->
    Just . EdMembersJoin . SimpleMembers <$> traverse mapSimpleMember simpleMembers

mapSimpleMember :: Fed.SimpleMember -> Galley SimpleMember
mapSimpleMember m =
  SimpleMember
    <$> (makeMappedIdOpaque . idMappingLocal <$> createUserIdMapping (Fed.smId m))
    <*> pure (mapConversationRole (Fed.smConversationRole m))

mapConversationRole :: Fed.ConversationRole -> RoleName
mapConversationRole = \case
  Fed.ConversationRoleAdmin -> roleNameWireAdmin
  Fed.ConversationRoleMember -> roleNameWireMember
