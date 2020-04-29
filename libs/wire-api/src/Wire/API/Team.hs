{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Team
  ( Team,
    TeamBinding (..),
    newTeam,
    teamId,
    teamCreator,
    teamName,
    teamIcon,
    teamIconKey,
    teamBinding,
    TeamCreationTime (..),
    tcTime,
    FeatureFlags (..),
    flagSSO,
    flagLegalHold,
    FeatureSSO (..),
    FeatureLegalHold (..),
    TeamList,
    newTeamList,
    teamListTeams,
    teamListHasMore,
    TeamMember,
    newTeamMember,
    newTeamMemberRaw,
    userId,
    permissions,
    invitation,
    legalHoldStatus,
    teamMemberJson,
    canSeePermsOf,
    TeamMemberList,
    ListType (..),
    notTeamMember,
    findTeamMember,
    isTeamMember,
    newTeamMemberList,
    teamMembers,
    teamMemberListType,
    teamMemberListJson,
    TeamConversation,
    newTeamConversation,
    conversationId,
    managedConversation,
    TeamConversationList,
    newTeamConversationList,
    teamConversations,
    Permissions,
    newPermissions,
    fullPermissions,
    noPermissions,
    serviceWhitelistPermissions,
    hasPermission,
    mayGrantPermission,
    isTeamOwner,
    self,
    copy,
    Perm (..),
    permToInt,
    permsToInt,
    intToPerm,
    intToPerms,
    HiddenPerm (..),
    IsPerm,
    Role (..),
    defaultRole,
    rolePermissions,
    permissionsRole,
    BindingNewTeam (..),
    NonBindingNewTeam (..),
    NewTeam,
    newNewTeam,
    newTeamName,
    newTeamIcon,
    newTeamIconKey,
    newTeamMembers,
    NewTeamMember,
    newNewTeamMember,
    ntmNewTeamMember,
    Event,
    newEvent,
    eventType,
    eventTime,
    eventTeam,
    eventData,
    EventType (..),
    EventData (..),
    TeamUpdateData,
    newTeamUpdateData,
    nameUpdate,
    iconUpdate,
    iconKeyUpdate,
    TeamMemberDeleteData,
    tmdAuthPassword,
    newTeamMemberDeleteData,
    TeamDeleteData,
    tdAuthPassword,
    newTeamDeleteData,
    HardTruncationLimit,
    hardTruncationLimit,
  )
where

import qualified Cassandra as Cql
import qualified Control.Error.Util as Err
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens ((^.), makeLenses, to, view)
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Bits ((.|.), testBit)
import qualified Data.HashMap.Strict as HashMap
import Data.Id (ConvId, TeamId, UserId)
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..))
import qualified Data.Maybe as Maybe
import Data.Misc (PlainTextPassword (..))
import Data.Proxy
import Data.Range
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Time (UTCTime)
import GHC.TypeLits
import Imports
import Wire.API.Team.Internal

data Event = Event
  { _eventType :: EventType,
    _eventTeam :: TeamId,
    _eventTime :: UTCTime,
    _eventData :: Maybe EventData
  }
  deriving (Eq, Generic)

-- Note [whitelist events]
-- ~~~~~~~~~~~~~~~
--
-- When a service is put off the whitelist, we want to notify users about
-- this so that they would be able to update their whitelists in real time
-- (or at least this could be useful for the team admin console). For this
-- we might eventually have events 'ServiceWhitelistAdd' and
-- 'ServiceWhitelistRemove'.
--
-- However, they're not really necessary for clients, and currently
-- implementing them is problematic. There are three choices and all are
-- bad:
--
--   1. If we decide to send them to all users, this would be an expensive
--      operation – especially once we have bigger teams. We can send these
--      events asynchonously, but it's still somewhat painful.
--
--   2. If we decide to only send them to e.g. team admins, now we have to
--      figure out who *are* team admins, and currently the backend doesn't
--      have a notion of a team admin at all. See Note [team roles].
--
--   3. We could create a new permission (e.g. "CanWhitelistServices") and
--      only send the event to users with this permission, because
--      presumably only they care about it. However, we can't do this
--      either, because adding new permissions is tricky.
--      See Note [team roles] again.
--
-- So, we don't send these events at all. An implementation was done, but
-- then removed in commit b4d777ede1c7f73e42b2e1bc356ce7346e0355bc.
--
-- It's also unclear whether these event types belong in Brig or in Galley;
-- arguably the code would be simpler if they were in Brig, so we should
-- think about that if we want to get them in.

data EventType
  = TeamCreate
  | TeamDelete
  | TeamUpdate
  | MemberJoin
  | MemberLeave
  | MemberUpdate
  | ConvCreate
  | ConvDelete
  deriving (Eq, Show, Generic)

data EventData
  = EdTeamCreate Team
  | EdTeamUpdate TeamUpdateData
  | EdMemberJoin UserId
  | EdMemberLeave UserId
  | EdMemberUpdate UserId (Maybe Permissions)
  | EdConvCreate ConvId
  | EdConvDelete ConvId
  deriving (Eq, Show, Generic)

data TeamUpdateData = TeamUpdateData
  { _nameUpdate :: Maybe (Range 1 256 Text),
    _iconUpdate :: Maybe (Range 1 256 Text),
    _iconKeyUpdate :: Maybe (Range 1 256 Text)
  }
  deriving (Eq, Show, Generic)

data TeamList = TeamList
  { _teamListTeams :: [Team],
    _teamListHasMore :: Bool
  }
  deriving (Show, Generic)

data TeamMember = TeamMember
  { _userId :: UserId,
    _permissions :: Permissions,
    _invitation :: Maybe (UserId, UTCTimeMillis),
    _legalHoldStatus :: UserLegalHoldStatus
  }
  deriving (Eq, Ord, Show, Generic)

data ListType
  = ListComplete
  | ListTruncated
  deriving (Eq, Show, Generic)

data TeamMemberList = TeamMemberList
  { _teamMembers :: [TeamMember],
    _teamMemberListType :: ListType
  }
  deriving (Generic)

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: Integral a => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

data TeamConversation = TeamConversation
  { _conversationId :: ConvId,
    _managedConversation :: Bool
  }

newtype TeamConversationList = TeamConversationList
  { _teamConversations :: [TeamConversation]
  }

data Permissions = Permissions
  { _self :: Set Perm,
    _copy :: Set Perm
  }
  deriving (Eq, Ord, Show, Generic)

-- | Team-level permission.  Analog to conversation-level 'Action'.
data Perm
  = CreateConversation
  | DoNotUseDeprecatedDeleteConversation -- NOTE: This gets now overruled by conv level checks
  | AddTeamMember
  | RemoveTeamMember
  | DoNotUseDeprecatedAddRemoveConvMember -- NOTE: This gets now overruled by conv level checks
  | DoNotUseDeprecatedModifyConvName -- NOTE: This gets now overruled by conv level checks
  | GetBilling
  | SetBilling
  | SetTeamData
  | GetMemberPermissions
  | SetMemberPermissions
  | GetTeamConversations
  | DeleteTeam
  -- FUTUREWORK: make the verbs in the roles more consistent
  -- (CRUD vs. Add,Remove vs; Get,Set vs. Create,Delete etc).
  -- If you ever think about adding a new permission flag,
  -- read Note [team roles] first.
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Team-level role.  Analog to conversation-level 'ConversationRole'.
data Role = RoleOwner | RoleAdmin | RoleMember | RoleExternalPartner
  deriving (Eq, Show, Enum, Bounded, Generic)

defaultRole :: Role
defaultRole = RoleMember

rolePermissions :: Role -> Permissions
rolePermissions role = Permissions p p where p = rolePerms role

permissionsRole :: Permissions -> Maybe Role
permissionsRole (Permissions p p') | p /= p' = Nothing
permissionsRole (Permissions p _) = permsRole p
  where
    permsRole :: Set Perm -> Maybe Role
    permsRole perms =
      Maybe.listToMaybe
        [role | role <- [minBound ..], rolePerms role == perms]

-- | Internal function for 'rolePermissions'.  (It works iff the two sets in 'Permissions' are
-- identical for every 'Role', otherwise it'll need to be specialized for the resp. sides.)
rolePerms :: Role -> Set Perm
rolePerms RoleOwner =
  rolePerms RoleAdmin
    <> Set.fromList
      [ GetBilling,
        SetBilling,
        DeleteTeam
      ]
rolePerms RoleAdmin =
  rolePerms RoleMember
    <> Set.fromList
      [ AddTeamMember,
        RemoveTeamMember,
        SetTeamData,
        SetMemberPermissions
      ]
rolePerms RoleMember =
  rolePerms RoleExternalPartner
    <> Set.fromList
      [ DoNotUseDeprecatedDeleteConversation,
        DoNotUseDeprecatedAddRemoveConvMember,
        DoNotUseDeprecatedModifyConvName,
        GetMemberPermissions
      ]
rolePerms RoleExternalPartner =
  Set.fromList
    [ CreateConversation,
      GetTeamConversations
    ]

newtype BindingNewTeam = BindingNewTeam (NewTeam ())
  deriving (Eq, Show, Generic)

-- | FUTUREWORK: this is dead code!  remove!
newtype NonBindingNewTeam = NonBindingNewTeam (NewTeam (Range 1 127 [TeamMember]))
  deriving (Eq, Show, Generic)

newtype NewTeamMember = NewTeamMember
  { _ntmNewTeamMember :: TeamMember
  }

newtype TeamMemberDeleteData = TeamMemberDeleteData
  { _tmdAuthPassword :: Maybe PlainTextPassword
  }

newtype TeamDeleteData = TeamDeleteData
  { _tdAuthPassword :: Maybe PlainTextPassword
  }

-- This is the cassandra timestamp of writetime(binding)
newtype TeamCreationTime = TeamCreationTime
  { _tcTime :: Int64
  }

data FeatureFlags = FeatureFlags
  { _flagSSO :: !FeatureSSO,
    _flagLegalHold :: !FeatureLegalHold
  }
  deriving (Eq, Show, Generic)

data FeatureSSO
  = FeatureSSOEnabledByDefault
  | FeatureSSODisabledByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data FeatureLegalHold
  = FeatureLegalHoldDisabledPermanently
  | FeatureLegalHoldDisabledByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON FeatureFlags where
  parseJSON = withObject "FeatureFlags" $ \obj ->
    FeatureFlags
      <$> (obj .: "sso")
      <*> (obj .: "legalhold")

instance ToJSON FeatureFlags where
  toJSON (FeatureFlags sso legalhold) =
    object $
      [ "sso" .= sso,
        "legalhold" .= legalhold
      ]

instance FromJSON FeatureSSO where
  parseJSON (String "enabled-by-default") = pure FeatureSSOEnabledByDefault
  parseJSON (String "disabled-by-default") = pure FeatureSSODisabledByDefault
  parseJSON bad = fail $ "FeatureSSO: " <> cs (encode bad)

instance ToJSON FeatureSSO where
  toJSON FeatureSSOEnabledByDefault = String "enabled-by-default"
  toJSON FeatureSSODisabledByDefault = String "disabled-by-default"

instance FromJSON FeatureLegalHold where
  parseJSON (String "disabled-permanently") = pure $ FeatureLegalHoldDisabledPermanently
  parseJSON (String "disabled-by-default") = pure $ FeatureLegalHoldDisabledByDefault
  parseJSON bad = fail $ "FeatureLegalHold: " <> cs (encode bad)

instance ToJSON FeatureLegalHold where
  toJSON FeatureLegalHoldDisabledPermanently = String "disabled-permanently"
  toJSON FeatureLegalHoldDisabledByDefault = String "disabled-by-default"

-- This replaces the previous `hasMore` but has no boolean blindness. At the API level
-- though we do want this to remain true/false
instance ToJSON ListType where
  toJSON ListComplete = Bool False
  toJSON ListTruncated = Bool True

instance FromJSON ListType where
  parseJSON (Bool False) = pure ListComplete
  parseJSON (Bool True) = pure ListTruncated
  parseJSON bad = fail $ "ListType: " <> cs (encode bad)

newTeam :: TeamId -> UserId -> Text -> Text -> TeamBinding -> Team
newTeam tid uid nme ico bnd = Team tid uid nme ico Nothing bnd

newTeamList :: [Team] -> Bool -> TeamList
newTeamList = TeamList

newTeamMember ::
  UserId ->
  Permissions ->
  Maybe (UserId, UTCTimeMillis) ->
  TeamMember
newTeamMember uid perm invitation = TeamMember uid perm invitation UserLegalHoldDisabled

-- | For being called in "Galley.Data".  Throws an exception if one of invitation timestamp
-- and inviter is 'Nothing' and the other is 'Just', which can only be caused by inconsistent
-- database content.
-- FUTUREWORK: We should do a DB scan and check whether this is _ever_ the case. This logic could
-- be applied to anything that we store in Cassandra
newTeamMemberRaw ::
  MonadThrow m =>
  UserId ->
  Permissions ->
  Maybe UserId ->
  Maybe UTCTimeMillis ->
  UserLegalHoldStatus ->
  m TeamMember
newTeamMemberRaw uid perms (Just invu) (Just invt) lhStatus =
  pure $ TeamMember uid perms (Just (invu, invt)) lhStatus
newTeamMemberRaw uid perms Nothing Nothing lhStatus =
  pure $ TeamMember uid perms Nothing lhStatus
newTeamMemberRaw _ _ _ _ _ = throwM $ ErrorCall "TeamMember with incomplete metadata."

newTeamMemberList :: [TeamMember] -> ListType -> TeamMemberList
newTeamMemberList = TeamMemberList

newTeamConversation :: ConvId -> Bool -> TeamConversation
newTeamConversation = TeamConversation

newTeamConversationList :: [TeamConversation] -> TeamConversationList
newTeamConversationList = TeamConversationList

newNewTeam :: Range 1 256 Text -> Range 1 256 Text -> NewTeam a
newNewTeam nme ico = NewTeam nme ico Nothing Nothing

newNewTeamMember :: TeamMember -> NewTeamMember
newNewTeamMember = NewTeamMember

newEvent :: EventType -> TeamId -> UTCTime -> Event
newEvent typ tid tme = Event typ tid tme Nothing

newTeamUpdateData :: TeamUpdateData
newTeamUpdateData = TeamUpdateData Nothing Nothing Nothing

newTeamMemberDeleteData :: Maybe PlainTextPassword -> TeamMemberDeleteData
newTeamMemberDeleteData = TeamMemberDeleteData

newTeamDeleteData :: Maybe PlainTextPassword -> TeamDeleteData
newTeamDeleteData = TeamDeleteData

makeLenses ''Team

makeLenses ''TeamList

makeLenses ''TeamMember

makeLenses ''TeamMemberList

makeLenses ''TeamConversation

makeLenses ''TeamConversationList

makeLenses ''Permissions

makeLenses ''NewTeam

makeLenses ''NewTeamMember

makeLenses ''Event

makeLenses ''TeamUpdateData

makeLenses ''TeamMemberDeleteData

makeLenses ''TeamDeleteData

makeLenses ''TeamCreationTime

makeLenses ''FeatureFlags

-- Note [hidden team roles]
--
-- The problem: the mapping between 'Role' and 'Permissions' is fixed by external contracts:
-- client apps treat permission bit matrices as opaque role identifiers, so if we add new
-- permission flags, things will break there.
--
-- The solution: add new permission bits to 'HiddenPerm', 'HiddenPermissions', and make
-- 'hasPermission', 'mayGrantPermission' polymorphic.  Now you can check both for the hidden
-- permission bits and the old ones that we share with the client apps.

-- | See Note [hidden team roles]
data HiddenPerm
  = ChangeLegalHoldTeamSettings
  | ViewLegalHoldTeamSettings
  | ChangeLegalHoldUserSettings
  | ViewLegalHoldUserSettings
  | ViewSSOTeamSettings -- (change is only allowed via customer support backoffice)
  | ViewSameTeamEmails
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | See Note [hidden team roles]
data HiddenPermissions = HiddenPermissions
  { _hself :: Set HiddenPerm,
    _hcopy :: Set HiddenPerm
  }
  deriving (Eq, Ord, Show)

makeLenses ''HiddenPermissions

-- | Compute 'Role' from 'Permissions', and 'HiddenPermissions' from the 'Role'.  If
-- 'Permissions' matches no 'Role', return no hidden permission bits.
hiddenPermissionsFromPermissions :: Permissions -> HiddenPermissions
hiddenPermissionsFromPermissions =
  maybe (HiddenPermissions mempty mempty) roleHiddenPermissions . permissionsRole
  where
    roleHiddenPermissions :: Role -> HiddenPermissions
    roleHiddenPermissions role = HiddenPermissions p p
      where
        p = roleHiddenPerms role
        roleHiddenPerms :: Role -> Set HiddenPerm
        roleHiddenPerms RoleOwner = roleHiddenPerms RoleAdmin
        roleHiddenPerms RoleAdmin =
          (roleHiddenPerms RoleMember <>) $
            Set.fromList
              [ ChangeLegalHoldTeamSettings,
                ChangeLegalHoldUserSettings
              ]
        roleHiddenPerms RoleMember =
          (roleHiddenPerms RoleExternalPartner <>) $
            Set.fromList [ViewSameTeamEmails]
        roleHiddenPerms RoleExternalPartner =
          Set.fromList
            [ ViewLegalHoldTeamSettings,
              ViewLegalHoldUserSettings,
              ViewSSOTeamSettings
            ]

-- | See Note [hidden team roles]
class IsPerm perm where
  hasPermission :: TeamMember -> perm -> Bool
  mayGrantPermission :: TeamMember -> perm -> Bool

instance IsPerm Perm where
  hasPermission tm p = p `Set.member` (tm ^. permissions . self)
  mayGrantPermission tm p = p `Set.member` (tm ^. permissions . copy)

instance IsPerm HiddenPerm where
  hasPermission tm p =
    p `Set.member` (tm ^. permissions . to hiddenPermissionsFromPermissions . hself)
  mayGrantPermission tm p =
    p `Set.member` (tm ^. permissions . to hiddenPermissionsFromPermissions . hcopy)

notTeamMember :: [UserId] -> [TeamMember] -> [UserId]
notTeamMember uids tmms =
  Set.toList $
    Set.fromList uids `Set.difference` Set.fromList (map (view userId) tmms)

isTeamMember :: Foldable m => UserId -> m TeamMember -> Bool
isTeamMember u = isJust . findTeamMember u

findTeamMember :: Foldable m => UserId -> m TeamMember -> Maybe TeamMember
findTeamMember u = find ((u ==) . view userId)

newPermissions ::
  -- | User's permissions
  Set Perm ->
  -- | Permissions that the user will be able to
  --   grant to other users (must be a subset)
  Set Perm ->
  Maybe Permissions
newPermissions a b
  | b `Set.isSubsetOf` a = Just (Permissions a b)
  | otherwise = Nothing

fullPermissions :: Permissions
fullPermissions = let p = intToPerms maxBound in Permissions p p

noPermissions :: Permissions
noPermissions = Permissions mempty mempty

-- | Permissions that a user needs to be considered a "service whitelist
-- admin" (can add and remove services from the whitelist).
serviceWhitelistPermissions :: Set Perm
serviceWhitelistPermissions =
  Set.fromList
    [ AddTeamMember,
      RemoveTeamMember,
      DoNotUseDeprecatedAddRemoveConvMember,
      SetTeamData
    ]

-- Note [team roles]
-- ~~~~~~~~~~~~
--
-- Client apps have a notion of *team roles*. They are defined as sets of
-- permissions:
--
--     member =
--         {AddRemoveConvMember, Create/DeleteConversation,
--         GetMemberPermissions, GetTeamConversations}
--
--     admin = member +
--         {Add/RemoveTeamMember, SetMemberPermissions, SetTeamData}
--
--     owner = admin +
--         {DeleteTeam, Get/SetBilling}
--
-- For instance, here: https://github.com/wireapp/wire-webapp/blob/dev/app/script/team/TeamPermission.js
--
-- Whenever a user has one of those specific sets of permissions, they are
-- considered a member/admin/owner and the client treats them accordingly
-- (e.g. for an admin it might show a certain button, while for an ordinary
-- user it won't).
--
-- On the backend, however, we don't have such a notion. Instead we have
-- granular (in fact, probably *too* granular) permission masks. Look at
-- 'Perm' and 'Permissions'.
--
-- Admins as a concept don't exist at all, and team owners are defined as
-- "full bitmask". When we do checks like "the backend must not let the last
-- team owner leave the team", this is what we test for. We also never test
-- for "team admin", and instead look at specific permissions.
--
-- Creating a new permission flag is thus very tricky, because if we decide
-- that all team admins must have this new permission, we will have to
-- identify all existing team admins. And if it turns out that some users
-- don't fit into one of those three team roles, we're screwed.

isTeamOwner :: TeamMember -> Bool
isTeamOwner tm = fullPermissions == (tm ^. permissions)

permToInt :: Perm -> Word64
permToInt CreateConversation = 0x0001
permToInt DoNotUseDeprecatedDeleteConversation = 0x0002
permToInt AddTeamMember = 0x0004
permToInt RemoveTeamMember = 0x0008
permToInt DoNotUseDeprecatedAddRemoveConvMember = 0x0010
permToInt DoNotUseDeprecatedModifyConvName = 0x0020
permToInt GetBilling = 0x0040
permToInt SetBilling = 0x0080
permToInt SetTeamData = 0x0100
permToInt GetMemberPermissions = 0x0200
permToInt GetTeamConversations = 0x0400
permToInt DeleteTeam = 0x0800
permToInt SetMemberPermissions = 0x1000

intToPerm :: Word64 -> Maybe Perm
intToPerm 0x0001 = Just CreateConversation
intToPerm 0x0002 = Just DoNotUseDeprecatedDeleteConversation
intToPerm 0x0004 = Just AddTeamMember
intToPerm 0x0008 = Just RemoveTeamMember
intToPerm 0x0010 = Just DoNotUseDeprecatedAddRemoveConvMember
intToPerm 0x0020 = Just DoNotUseDeprecatedModifyConvName
intToPerm 0x0040 = Just GetBilling
intToPerm 0x0080 = Just SetBilling
intToPerm 0x0100 = Just SetTeamData
intToPerm 0x0200 = Just GetMemberPermissions
intToPerm 0x0400 = Just GetTeamConversations
intToPerm 0x0800 = Just DeleteTeam
intToPerm 0x1000 = Just SetMemberPermissions
intToPerm _ = Nothing

intToPerms :: Word64 -> Set Perm
intToPerms n =
  let perms = [2 ^ i | i <- [0 .. 62], n `testBit` i]
   in Set.fromList (mapMaybe intToPerm perms)

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0

instance ToJSON TeamList where
  toJSON t =
    object $
      "teams" .= _teamListTeams t
        # "has_more" .= _teamListHasMore t
        # []

instance FromJSON TeamList where
  parseJSON = withObject "teamlist" $ \o -> do
    TeamList <$> o .: "teams"
      <*> o .: "has_more"

instance ToJSON TeamMember where
  toJSON = teamMemberJson (const True)

-- | Show 'Permissions' conditionally.  The condition takes the member that will receive the result
-- into account.  See 'canSeePermsOf'.
teamMemberJson :: (TeamMember -> Bool) -> TeamMember -> Value
teamMemberJson withPerms m =
  object $
    ["user" .= _userId m]
      <> ["permissions" .= _permissions m | withPerms m]
      <> ["created_by" .= (fst <$> _invitation m)]
      <> ["created_at" .= (snd <$> _invitation m)]
      <> ["legalhold_status" .= _legalHoldStatus m]

-- | Use this to construct the condition expected by 'teamMemberJson', 'teamMemberListJson'
canSeePermsOf :: TeamMember -> TeamMember -> Bool
canSeePermsOf seeer seeee =
  seeer `hasPermission` GetMemberPermissions || seeer == seeee

parseTeamMember :: Value -> Parser TeamMember
parseTeamMember = withObject "team-member" $ \o ->
  TeamMember <$> o .: "user"
    <*> o .: "permissions"
    <*> parseInvited o
    -- Default to disabled if missing
    <*> o .:? "legalhold_status" .!= UserLegalHoldDisabled
  where
    parseInvited :: Object -> Parser (Maybe (UserId, UTCTimeMillis))
    parseInvited o = do
      invby <- o .:? "created_by"
      invat <- o .:? "created_at"
      case (invby, invat) of
        (Just b, Just a) -> pure $ Just (b, a)
        (Nothing, Nothing) -> pure $ Nothing
        _ -> fail "created_by, created_at"

instance ToJSON TeamMemberList where
  toJSON = teamMemberListJson (const True)

-- | Show a list of team members using 'teamMemberJson'.
teamMemberListJson :: (TeamMember -> Bool) -> TeamMemberList -> Value
teamMemberListJson withPerms l =
  object
    [ "members" .= map (teamMemberJson withPerms) (_teamMembers l),
      "hasMore" .= _teamMemberListType l
    ]

instance FromJSON TeamMember where
  parseJSON = parseTeamMember

instance FromJSON TeamMemberList where
  parseJSON = withObject "team member list" $ \o ->
    TeamMemberList <$> o .: "members" <*> o .: "hasMore"

instance ToJSON TeamConversation where
  toJSON t =
    object
      [ "conversation" .= _conversationId t,
        "managed" .= _managedConversation t
      ]

instance FromJSON TeamConversation where
  parseJSON = withObject "team conversation" $ \o ->
    TeamConversation <$> o .: "conversation" <*> o .: "managed"

instance ToJSON TeamConversationList where
  toJSON t = object ["conversations" .= _teamConversations t]

instance FromJSON TeamConversationList where
  parseJSON = withObject "team conversation list" $ \o -> do
    TeamConversationList <$> o .: "conversations"

instance ToJSON Permissions where
  toJSON p =
    object $
      "self" .= permsToInt (_self p)
        # "copy" .= permsToInt (_copy p)
        # []

instance FromJSON Permissions where
  parseJSON = withObject "permissions" $ \o -> do
    s <- intToPerms <$> o .: "self"
    d <- intToPerms <$> o .: "copy"
    case newPermissions s d of
      Nothing -> fail "invalid permissions"
      Just ps -> pure ps

instance ToJSON Role where
  toJSON RoleOwner = "owner"
  toJSON RoleAdmin = "admin"
  toJSON RoleMember = "member"
  toJSON RoleExternalPartner = "partner"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "owner" -> pure RoleOwner
    "admin" -> pure RoleAdmin
    "member" -> pure RoleMember
    "partner" -> pure RoleExternalPartner
    "collaborator" -> pure RoleExternalPartner
    -- 'collaborator' was used for a short period of time on staging.  if you are
    -- wondering about this, it's probably safe to remove.
    -- ~fisx, Wed Jan 23 16:38:52 CET 2019
    bad -> fail $ "not a role: " <> show bad

newTeamJson :: NewTeam a -> [Pair]
newTeamJson (NewTeam n i ik _) =
  "name" .= fromRange n
    # "icon" .= fromRange i
    # "icon_key" .= (fromRange <$> ik)
    # []

instance ToJSON BindingNewTeam where
  toJSON (BindingNewTeam t) = object $ newTeamJson t

instance ToJSON NonBindingNewTeam where
  toJSON (NonBindingNewTeam t) =
    object $
      "members" .= (fromRange <$> _newTeamMembers t)
        # newTeamJson t

deriving instance FromJSON BindingNewTeam

deriving instance FromJSON NonBindingNewTeam

instance ToJSON NewTeamMember where
  toJSON t = object ["member" .= _ntmNewTeamMember t]

instance FromJSON NewTeamMember where
  parseJSON = withObject "add team member" $ \o ->
    NewTeamMember <$> o .: "member"

instance ToJSON EventType where
  toJSON TeamCreate = String "team.create"
  toJSON TeamDelete = String "team.delete"
  toJSON TeamUpdate = String "team.update"
  toJSON MemberJoin = String "team.member-join"
  toJSON MemberUpdate = String "team.member-update"
  toJSON MemberLeave = String "team.member-leave"
  toJSON ConvCreate = String "team.conversation-create"
  toJSON ConvDelete = String "team.conversation-delete"

instance FromJSON EventType where
  parseJSON (String "team.create") = pure TeamCreate
  parseJSON (String "team.delete") = pure TeamDelete
  parseJSON (String "team.update") = pure TeamUpdate
  parseJSON (String "team.member-join") = pure MemberJoin
  parseJSON (String "team.member-update") = pure MemberUpdate
  parseJSON (String "team.member-leave") = pure MemberLeave
  parseJSON (String "team.conversation-create") = pure ConvCreate
  parseJSON (String "team.conversation-delete") = pure ConvDelete
  parseJSON other = fail $ "Unknown event type: " <> show other

instance ToJSON Event where
  toJSON = Object . toJSONObject

instance ToJSONObject Event where
  toJSONObject e =
    HashMap.fromList
      [ "type" .= _eventType e,
        "team" .= _eventTeam e,
        "time" .= _eventTime e,
        "data" .= _eventData e
      ]

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    ty <- o .: "type"
    dt <- o .:? "data"
    Event ty <$> o .: "team"
      <*> o .: "time"
      <*> parseEventData ty dt

instance ToJSON EventData where
  toJSON (EdTeamCreate tem) = toJSON tem
  toJSON (EdMemberJoin usr) = object ["user" .= usr]
  toJSON (EdMemberUpdate usr mPerm) =
    object $
      "user" .= usr
        # "permissions" .= mPerm
        # []
  toJSON (EdMemberLeave usr) = object ["user" .= usr]
  toJSON (EdConvCreate cnv) = object ["conv" .= cnv]
  toJSON (EdConvDelete cnv) = object ["conv" .= cnv]
  toJSON (EdTeamUpdate upd) = toJSON upd

parseEventData :: EventType -> Maybe Value -> Parser (Maybe EventData)
parseEventData MemberJoin Nothing = fail "missing event data for type 'team.member-join'"
parseEventData MemberJoin (Just j) = do
  let f o = Just . EdMemberJoin <$> o .: "user"
  withObject "member join data" f j
parseEventData MemberUpdate Nothing = fail "missing event data for type 'team.member-update"
parseEventData MemberUpdate (Just j) = do
  let f o = Just <$> (EdMemberUpdate <$> o .: "user" <*> o .:? "permissions")
  withObject "member update data" f j
parseEventData MemberLeave Nothing = fail "missing event data for type 'team.member-leave'"
parseEventData MemberLeave (Just j) = do
  let f o = Just . EdMemberLeave <$> o .: "user"
  withObject "member leave data" f j
parseEventData ConvCreate Nothing = fail "missing event data for type 'team.conversation-create"
parseEventData ConvCreate (Just j) = do
  let f o = Just . EdConvCreate <$> o .: "conv"
  withObject "conversation create data" f j
parseEventData ConvDelete Nothing = fail "missing event data for type 'team.conversation-delete"
parseEventData ConvDelete (Just j) = do
  let f o = Just . EdConvDelete <$> o .: "conv"
  withObject "conversation delete data" f j
parseEventData TeamCreate Nothing = fail "missing event data for type 'team.create'"
parseEventData TeamCreate (Just j) = Just . EdTeamCreate <$> parseJSON j
parseEventData TeamUpdate Nothing = fail "missing event data for type 'team.update'"
parseEventData TeamUpdate (Just j) = Just . EdTeamUpdate <$> parseJSON j
parseEventData _ Nothing = pure Nothing
parseEventData t (Just _) = fail $ "unexpected event data for type " <> show t

instance ToJSON TeamUpdateData where
  toJSON u =
    object $
      "name" .= _nameUpdate u
        # "icon" .= _iconUpdate u
        # "icon_key" .= _iconKeyUpdate u
        # []

instance FromJSON TeamUpdateData where
  parseJSON = withObject "team update data" $ \o -> do
    name <- o .:? "name"
    icon <- o .:? "icon"
    icon_key <- o .:? "icon_key"
    when (isNothing name && isNothing icon && isNothing icon_key) $
      fail "TeamUpdateData: no update data specified"
    either fail pure $
      TeamUpdateData <$> maybe (pure Nothing) (fmap Just . checkedEitherMsg "name") name
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon") icon
        <*> maybe (pure Nothing) (fmap Just . checkedEitherMsg "icon_key") icon_key

instance FromJSON TeamMemberDeleteData where
  parseJSON = withObject "team-member-delete-data" $ \o ->
    TeamMemberDeleteData <$> (o .:? "password")

instance ToJSON TeamMemberDeleteData where
  toJSON tmd =
    object
      [ "password" .= _tmdAuthPassword tmd
      ]

instance FromJSON TeamDeleteData where
  parseJSON = withObject "team-delete-data" $ \o ->
    TeamDeleteData <$> o .: "password"

instance ToJSON TeamDeleteData where
  toJSON tdd =
    object
      [ "password" .= _tdAuthPassword tdd
      ]

instance Cql.Cql Role where
  ctype = Cql.Tagged Cql.IntColumn

  toCql RoleOwner = Cql.CqlInt 1
  toCql RoleAdmin = Cql.CqlInt 2
  toCql RoleMember = Cql.CqlInt 3
  toCql RoleExternalPartner = Cql.CqlInt 4

  fromCql (Cql.CqlInt i) = case i of
    1 -> return RoleOwner
    2 -> return RoleAdmin
    3 -> return RoleMember
    4 -> return RoleExternalPartner
    n -> fail $ "Unexpected Role value: " ++ show n
  fromCql _ = fail "Role value: int expected"

instance Cql.Cql Permissions where
  ctype = Cql.Tagged $ Cql.UdtColumn "permissions" [("self", Cql.BigIntColumn), ("copy", Cql.BigIntColumn)]

  toCql p =
    let f = Cql.CqlBigInt . fromIntegral . permsToInt
     in Cql.CqlUdt [("self", f (p ^. self)), ("copy", f (p ^. copy))]

  fromCql (Cql.CqlUdt p) = do
    let f = intToPerms . fromIntegral :: Int64 -> Set.Set Perm
    s <- Err.note "missing 'self' permissions" ("self" `lookup` p) >>= Cql.fromCql
    d <- Err.note "missing 'copy' permissions" ("copy" `lookup` p) >>= Cql.fromCql
    r <- Err.note "invalid permissions" (newPermissions (f s) (f d))
    pure r
  fromCql _ = fail "permissions: udt expected"
