{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Galley.Types.Teams
  ( TeamCreationTime (..),
    tcTime,
    FeatureFlags (..),
    flagSSO,
    flagLegalHold,
    flagTeamSearchVisibility,
    FeatureSSO (..),
    FeatureLegalHold (..),
    FeatureTeamSearchVisibility (..),
    newTeamMemberRaw,
    notTeamMember,
    findTeamMember,
    isTeamMember,
    isTeamOwner,
    canSeePermsOf,
    rolePermissions,
    roleHiddenPermissions,
    permissionsRole,
    HiddenPerm (..),
    IsPerm (..),

    -- * re-exports
    Team,
    TeamBinding (..),
    newTeam,
    teamId,
    teamCreator,
    teamName,
    teamIcon,
    teamIconKey,
    teamBinding,
    TeamList,
    newTeamList,
    teamListTeams,
    teamListHasMore,
    TeamMember,
    newTeamMember,
    userId,
    permissions,
    invitation,
    legalHoldStatus,
    teamMemberJson,
    TeamMemberList,
    ListType (..),
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
    self,
    copy,
    Perm (..),
    permToInt,
    permsToInt,
    intToPerm,
    intToPerms,
    Role (..),
    defaultRole,
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

import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens (makeLenses, view, (^.))
import Control.Monad.Catch
import Data.Aeson
import Data.Id (UserId)
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Imports
import Wire.API.Event.Team
import Wire.API.Team
import Wire.API.Team (NewTeam (..), Team (..), TeamBinding (..))
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role

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

-- This is the cassandra timestamp of writetime(binding)
newtype TeamCreationTime = TeamCreationTime
  { _tcTime :: Int64
  }

data FeatureFlags = FeatureFlags
  { _flagSSO :: !FeatureSSO,
    _flagLegalHold :: !FeatureLegalHold,
    _flagTeamSearchVisibility :: !FeatureTeamSearchVisibility
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

-- | Default value for all teams that have not enabled or disabled this feature explicitly.
-- See also 'Wire.API.Team.SearchVisibility.TeamSearchVisibilityEnabled',
-- 'Wire.API.Team.SearchVisibility.TeamSearchVisibility'.
data FeatureTeamSearchVisibility
  = FeatureTeamSearchVisibilityEnabledByDefault
  | FeatureTeamSearchVisibilityDisabledByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- NOTE: This is used only in the config and thus YAML... camelcase
instance FromJSON FeatureFlags where
  parseJSON = withObject "FeatureFlags" $ \obj ->
    FeatureFlags
      <$> obj .: "sso"
      <*> obj .: "legalhold"
      <*> obj .: "teamSearchVisibility"

instance ToJSON FeatureFlags where
  toJSON (FeatureFlags sso legalhold searchVisibility) =
    object $
      [ "sso" .= sso,
        "legalhold" .= legalhold,
        "teamSearchVisibility" .= searchVisibility
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

instance FromJSON FeatureTeamSearchVisibility where
  parseJSON (String "enabled-by-default") = pure FeatureTeamSearchVisibilityEnabledByDefault
  parseJSON (String "disabled-by-default") = pure FeatureTeamSearchVisibilityDisabledByDefault
  parseJSON bad = fail $ "FeatureSearchVisibility: " <> cs (encode bad)

instance ToJSON FeatureTeamSearchVisibility where
  toJSON FeatureTeamSearchVisibilityEnabledByDefault = String "enabled-by-default"
  toJSON FeatureTeamSearchVisibilityDisabledByDefault = String "disabled-by-default"

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
  | ChangeLegalHoldUserSettings
  | ViewLegalHoldUserSettings
  | ViewTeamFeature TeamFeatureName
  | ChangeTeamSearchVisibility
  | ViewTeamSearchVisibility
  | ViewSameTeamEmails
  deriving (Eq, Ord, Show)

-- | See Note [hidden team roles]
data HiddenPermissions = HiddenPermissions
  { _hself :: Set HiddenPerm,
    _hcopy :: Set HiddenPerm
  }
  deriving (Eq, Ord, Show)

makeLenses ''HiddenPermissions

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
            ChangeLegalHoldUserSettings,
            ChangeTeamSearchVisibility
          ]
    roleHiddenPerms RoleMember =
      (roleHiddenPerms RoleExternalPartner <>) $
        Set.fromList [ViewSameTeamEmails]
    roleHiddenPerms RoleExternalPartner =
      Set.fromList
        [ ViewTeamFeature TeamFeatureLegalHold,
          ViewTeamFeature TeamFeatureSSO,
          ViewTeamFeature TeamFeatureSearchVisibility,
          ViewTeamFeature TeamFeatureValidateSAMLEmails,
          ViewTeamFeature TeamFeatureDigitalSignatures,
          ViewLegalHoldUserSettings,
          ViewTeamSearchVisibility
        ]

-- | See Note [hidden team roles]
class IsPerm perm where
  roleHasPerm :: Role -> perm -> Bool
  roleGrantsPerm :: Role -> perm -> Bool
  hasPermission :: TeamMember -> perm -> Bool
  hasPermission tm perm = maybe False (`roleHasPerm` perm) . permissionsRole $ tm ^. permissions
  mayGrantPermission :: TeamMember -> perm -> Bool
  mayGrantPermission tm perm = maybe False (`roleGrantsPerm` perm) . permissionsRole $ tm ^. permissions

instance IsPerm Perm where
  roleHasPerm r p = p `Set.member` (rolePermissions r ^. self)
  roleGrantsPerm r p = p `Set.member` (rolePermissions r ^. copy)
  hasPermission tm p = p `Set.member` (tm ^. permissions . self)
  mayGrantPermission tm p = p `Set.member` (tm ^. permissions . copy)

instance IsPerm HiddenPerm where
  roleHasPerm r p = p `Set.member` (roleHiddenPermissions r ^. hself)
  roleGrantsPerm r p = p `Set.member` (roleHiddenPermissions r ^. hcopy)

notTeamMember :: [UserId] -> [TeamMember] -> [UserId]
notTeamMember uids tmms =
  Set.toList $
    Set.fromList uids `Set.difference` Set.fromList (map (view userId) tmms)

isTeamMember :: Foldable m => UserId -> m TeamMember -> Bool
isTeamMember u = isJust . findTeamMember u

findTeamMember :: Foldable m => UserId -> m TeamMember -> Maybe TeamMember
findTeamMember u = find ((u ==) . view userId)

isTeamOwner :: TeamMember -> Bool
isTeamOwner tm = fullPermissions == (tm ^. permissions)

-- | Use this to construct the condition expected by 'teamMemberJson', 'teamMemberListJson'
canSeePermsOf :: TeamMember -> TeamMember -> Bool
canSeePermsOf seeer seeee =
  seeer `hasPermission` GetMemberPermissions || seeer == seeee
