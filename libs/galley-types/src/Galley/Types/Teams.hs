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
    flagFileSharing,
    flagAppLockDefaults,
    flagClassifiedDomains,
    flagConferenceCalling,
    Defaults (..),
    unDefaults,
    FeatureSSO (..),
    FeatureLegalHold (..),
    FeatureTeamSearchVisibility (..),
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

import Control.Lens (makeLenses, view, (^.))
import Data.Aeson
import Data.Id (UserId)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Event.Team
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role

rolePermissions :: Role -> Permissions
rolePermissions role = Permissions p p where p = rolePerms role

permissionsRole :: Permissions -> Maybe Role
permissionsRole (Permissions p p') =
  if p /= p'
    then do
      -- we never did use @p /= p'@ for anything, fingers crossed that it doesn't occur anywhere
      -- in the wild.  but if it does, this implementation prevents privilege escalation.
      let p'' = Set.intersection p p'
       in permissionsRole (Permissions p'' p'')
    else permsRole p
  where
    permsRole :: Set Perm -> Maybe Role
    permsRole perms =
      Maybe.listToMaybe
        [ role
          | role <- [minBound ..],
            -- if a there is a role that is strictly less permissive than the perms set that
            -- we encounter, we downgrade.  this shouldn't happen in real life, but it has
            -- happened to very old users on a staging environment, where a user (probably)
            -- was create before the current publicly visible permissions had been stabilized.
            rolePerms role `Set.isSubsetOf` perms
        ]

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
    _flagTeamSearchVisibility :: !FeatureTeamSearchVisibility,
    _flagAppLockDefaults :: !(Defaults (TeamFeatureStatus 'TeamFeatureAppLock)),
    _flagClassifiedDomains :: !(TeamFeatureStatus 'TeamFeatureClassifiedDomains),
    _flagFileSharing :: !(Defaults (TeamFeatureStatus 'TeamFeatureFileSharing)),
    _flagConferenceCalling :: !(Defaults (TeamFeatureStatus 'TeamFeatureConferenceCalling))
  }
  deriving (Eq, Show, Generic)

newtype Defaults a = Defaults {_unDefaults :: a}
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving newtype (Arbitrary)

instance FromJSON a => FromJSON (Defaults a) where
  parseJSON = withObject "default object" $ \ob ->
    Defaults <$> (ob .: "defaults")

instance ToJSON a => ToJSON (Defaults a) where
  toJSON (Defaults x) =
    object ["defaults" .= toJSON x]

data FeatureSSO
  = FeatureSSOEnabledByDefault
  | FeatureSSODisabledByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data FeatureLegalHold
  = FeatureLegalHoldDisabledPermanently
  | FeatureLegalHoldDisabledByDefault
  | FeatureLegalHoldWhitelistTeamsAndImplicitConsent
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
      <*> (fromMaybe (Defaults defaultAppLockStatus) <$> (obj .:? "appLock"))
      <*> (fromMaybe defaultClassifiedDomains <$> (obj .:? "classifiedDomains"))
      <*> (fromMaybe (Defaults (TeamFeatureStatusNoConfig TeamFeatureEnabled)) <$> (obj .:? "fileSharing"))
      <*> (fromMaybe (Defaults (TeamFeatureStatusNoConfig TeamFeatureEnabled)) <$> (obj .:? "conferenceCalling"))

instance ToJSON FeatureFlags where
  toJSON (FeatureFlags sso legalhold searchVisibility appLock classifiedDomains fileSharing conferenceCalling) =
    object $
      [ "sso" .= sso,
        "legalhold" .= legalhold,
        "teamSearchVisibility" .= searchVisibility,
        "appLock" .= appLock,
        "classifiedDomains" .= classifiedDomains,
        "fileSharing" .= fileSharing,
        "conferenceCalling" .= conferenceCalling
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
  parseJSON (String "whitelist-teams-and-implicit-consent") = pure FeatureLegalHoldWhitelistTeamsAndImplicitConsent
  parseJSON bad = fail $ "FeatureLegalHold: " <> cs (encode bad)

instance ToJSON FeatureLegalHold where
  toJSON FeatureLegalHoldDisabledPermanently = String "disabled-permanently"
  toJSON FeatureLegalHoldDisabledByDefault = String "disabled-by-default"
  toJSON FeatureLegalHoldWhitelistTeamsAndImplicitConsent = String "whitelist-teams-and-implicit-consent"

instance FromJSON FeatureTeamSearchVisibility where
  parseJSON (String "enabled-by-default") = pure FeatureTeamSearchVisibilityEnabledByDefault
  parseJSON (String "disabled-by-default") = pure FeatureTeamSearchVisibilityDisabledByDefault
  parseJSON bad = fail $ "FeatureSearchVisibility: " <> cs (encode bad)

instance ToJSON FeatureTeamSearchVisibility where
  toJSON FeatureTeamSearchVisibilityEnabledByDefault = String "enabled-by-default"
  toJSON FeatureTeamSearchVisibilityDisabledByDefault = String "disabled-by-default"

makeLenses ''TeamCreationTime
makeLenses ''FeatureFlags
makeLenses ''Defaults

-- Note [hidden team roles]
--
-- The problem: the mapping between 'Role' and 'Permissions' is fixed by external contracts:
-- client apps treat permission bit matrices as opaque role identifiers, so if we add new
-- permission flags, things will break there.
--
-- "Hidden" in "HiddenPerm", therefore, refers to a permission hidden from
-- clients, thereby making it internal to the backend.
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
  | ChangeTeamFeature TeamFeatureName
  | ChangeTeamSearchVisibility
  | ViewTeamSearchVisibility
  | ViewSameTeamEmails
  | ReadIdp
  | CreateUpdateDeleteIdp
  | CreateReadDeleteScimToken
  | -- | this has its own permission because we're not sure how
    -- efficient this end-point is.  better not let all team members
    -- play with it unless we have to.
    DownloadTeamMembersCsv
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
            ChangeTeamSearchVisibility,
            ChangeTeamFeature TeamFeatureAppLock,
            ChangeTeamFeature TeamFeatureFileSharing,
            ChangeTeamFeature TeamFeatureClassifiedDomains {- the features not listed here can only be changed in stern -},
            ReadIdp,
            CreateUpdateDeleteIdp,
            CreateReadDeleteScimToken,
            DownloadTeamMembersCsv
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
          ViewTeamFeature TeamFeatureAppLock,
          ViewTeamFeature TeamFeatureFileSharing,
          ViewTeamFeature TeamFeatureClassifiedDomains,
          ViewTeamFeature TeamFeatureConferenceCalling,
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
