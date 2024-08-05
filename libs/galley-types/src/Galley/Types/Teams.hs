{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    flagSelfDeletingMessages,
    flagConversationGuestLinks,
    flagsTeamFeatureValidateSAMLEmailsStatus,
    flagTeamFeatureSndFactorPasswordChallengeStatus,
    flagTeamFeatureSearchVisibilityInbound,
    flagOutlookCalIntegration,
    flagMLS,
    flagMlsE2EId,
    flagMlsMigration,
    flagEnforceFileDownloadLocation,
    flagLimitedEventFanout,
    Defaults (..),
    ImplicitLockStatus (..),
    unImplicitLockStatus,
    unDefaults,
    FeatureSSO (..),
    FeatureLegalHold (..),
    FeatureTeamSearchVisibilityAvailability (..),
    notTeamMember,
    findTeamMember,
    isTeamMember,
    isTeamOwner,
    canSeePermsOf,
    rolePermissions,
    roleHiddenPermissions,
    permissionsRole,
    isAdminOrOwner,
    HiddenPerm (..),
    IsPerm (..),
  )
where

import Control.Lens (makeLenses, view, (^.))
import Data.Aeson
import Data.Aeson.Types qualified as A
import Data.Id (UserId)
import Data.Maybe qualified as Maybe
import Data.Schema qualified as Schema
import Data.Set qualified as Set
import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Error.Galley
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

isAdminOrOwner :: Permissions -> Bool
isAdminOrOwner perms =
  case permissionsRole perms of
    Just RoleOwner -> True
    Just RoleAdmin -> True
    Just RoleMember -> False
    Just RoleExternalPartner -> False
    Nothing -> False

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
      [ DeleteConversation,
        AddRemoveConvMember,
        ModifyConvName,
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
    _flagTeamSearchVisibility :: !FeatureTeamSearchVisibilityAvailability,
    _flagAppLockDefaults :: !(Defaults (ImplicitLockStatus AppLockConfig)),
    _flagClassifiedDomains :: !(ImplicitLockStatus ClassifiedDomainsConfig),
    _flagFileSharing :: !(Defaults (WithStatus FileSharingConfig)),
    _flagConferenceCalling :: !(Defaults (WithStatus ConferenceCallingConfig)),
    _flagSelfDeletingMessages :: !(Defaults (WithStatus SelfDeletingMessagesConfig)),
    _flagConversationGuestLinks :: !(Defaults (WithStatus GuestLinksConfig)),
    _flagsTeamFeatureValidateSAMLEmailsStatus :: !(Defaults (ImplicitLockStatus ValidateSAMLEmailsConfig)),
    _flagTeamFeatureSndFactorPasswordChallengeStatus :: !(Defaults (WithStatus SndFactorPasswordChallengeConfig)),
    _flagTeamFeatureSearchVisibilityInbound :: !(Defaults (ImplicitLockStatus SearchVisibilityInboundConfig)),
    _flagMLS :: !(Defaults (WithStatus MLSConfig)),
    _flagOutlookCalIntegration :: !(Defaults (WithStatus OutlookCalIntegrationConfig)),
    _flagMlsE2EId :: !(Defaults (WithStatus MlsE2EIdConfig)),
    _flagMlsMigration :: !(Defaults (WithStatus MlsMigrationConfig)),
    _flagEnforceFileDownloadLocation :: !(Defaults (WithStatus EnforceFileDownloadLocationConfig)),
    _flagLimitedEventFanout :: !(Defaults (ImplicitLockStatus LimitedEventFanoutConfig))
  }
  deriving (Eq, Show, Generic)

newtype Defaults a = Defaults {_unDefaults :: a}
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Functor)
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
data FeatureTeamSearchVisibilityAvailability
  = FeatureTeamSearchVisibilityAvailableByDefault
  | FeatureTeamSearchVisibilityUnavailableByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- NOTE: This is used only in the config and thus YAML... camelcase
instance FromJSON FeatureFlags where
  parseJSON = withObject "FeatureFlags" $ \obj ->
    FeatureFlags
      <$> obj .: "sso"
      <*> obj .: "legalhold"
      <*> obj .: "teamSearchVisibility"
      <*> withImplicitLockStatusOrDefault obj "appLock"
      <*> (fromMaybe (ImplicitLockStatus (defFeatureStatus @ClassifiedDomainsConfig)) <$> (obj .:? "classifiedDomains"))
      <*> (fromMaybe (Defaults (defFeatureStatus @FileSharingConfig)) <$> (obj .:? "fileSharing"))
      <*> (fromMaybe (Defaults (defFeatureStatus @ConferenceCallingConfig)) <$> (obj .:? "conferenceCalling"))
      <*> (fromMaybe (Defaults (defFeatureStatus @SelfDeletingMessagesConfig)) <$> (obj .:? "selfDeletingMessages"))
      <*> (fromMaybe (Defaults (defFeatureStatus @GuestLinksConfig)) <$> (obj .:? "conversationGuestLinks"))
      <*> withImplicitLockStatusOrDefault obj "validateSAMLEmails"
      <*> (fromMaybe (Defaults (defFeatureStatus @SndFactorPasswordChallengeConfig)) <$> (obj .:? "sndFactorPasswordChallenge"))
      <*> withImplicitLockStatusOrDefault obj "searchVisibilityInbound"
      <*> (fromMaybe (Defaults (defFeatureStatus @MLSConfig)) <$> (obj .:? "mls"))
      <*> (fromMaybe (Defaults (defFeatureStatus @OutlookCalIntegrationConfig)) <$> (obj .:? "outlookCalIntegration"))
      <*> (fromMaybe (Defaults (defFeatureStatus @MlsE2EIdConfig)) <$> (obj .:? "mlsE2EId"))
      <*> (fromMaybe (Defaults (defFeatureStatus @MlsMigrationConfig)) <$> (obj .:? "mlsMigration"))
      <*> (fromMaybe (Defaults (defFeatureStatus @EnforceFileDownloadLocationConfig)) <$> (obj .:? "enforceFileDownloadLocation"))
      <*> withImplicitLockStatusOrDefault obj "limitedEventFanout"
    where
      withImplicitLockStatusOrDefault :: forall cfg. (IsFeatureConfig cfg, Schema.ToSchema cfg) => Object -> Key -> A.Parser (Defaults (ImplicitLockStatus cfg))
      withImplicitLockStatusOrDefault obj fieldName = fromMaybe (Defaults (ImplicitLockStatus (defFeatureStatus @cfg))) <$> obj .:? fieldName

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

instance FromJSON FeatureTeamSearchVisibilityAvailability where
  parseJSON (String "enabled-by-default") = pure FeatureTeamSearchVisibilityAvailableByDefault
  parseJSON (String "disabled-by-default") = pure FeatureTeamSearchVisibilityUnavailableByDefault
  parseJSON bad = fail $ "FeatureSearchVisibility: " <> cs (encode bad)

instance ToJSON FeatureTeamSearchVisibilityAvailability where
  toJSON FeatureTeamSearchVisibilityAvailableByDefault = String "enabled-by-default"
  toJSON FeatureTeamSearchVisibilityUnavailableByDefault = String "disabled-by-default"

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
  | ChangeTeamFeature
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
  | ChangeTeamMemberProfiles
  | SearchContacts
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
            ChangeTeamFeature,
            ChangeTeamMemberProfiles,
            ReadIdp,
            CreateUpdateDeleteIdp,
            CreateReadDeleteScimToken,
            DownloadTeamMembersCsv
          ]
    roleHiddenPerms RoleMember =
      (roleHiddenPerms RoleExternalPartner <>) $
        Set.fromList
          [ ViewSameTeamEmails,
            SearchContacts
          ]
    roleHiddenPerms RoleExternalPartner =
      Set.fromList
        [ ViewLegalHoldUserSettings,
          ViewTeamSearchVisibility
        ]

-- | See Note [hidden team roles]
class IsPerm perm where
  type PermError (e :: perm) :: GalleyError

  roleHasPerm :: Role -> perm -> Bool
  roleGrantsPerm :: Role -> perm -> Bool
  hasPermission :: TeamMember -> perm -> Bool
  hasPermission tm perm = maybe False (`roleHasPerm` perm) . permissionsRole $ tm ^. permissions
  mayGrantPermission :: TeamMember -> perm -> Bool
  mayGrantPermission tm perm = maybe False (`roleGrantsPerm` perm) . permissionsRole $ tm ^. permissions

instance IsPerm Perm where
  type PermError p = 'MissingPermission ('Just p)

  roleHasPerm r p = p `Set.member` (rolePermissions r ^. self)
  roleGrantsPerm r p = p `Set.member` (rolePermissions r ^. copy)
  hasPermission tm p = p `Set.member` (tm ^. permissions . self)
  mayGrantPermission tm p = p `Set.member` (tm ^. permissions . copy)

instance IsPerm HiddenPerm where
  type PermError p = OperationDenied

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

isTeamOwner :: TeamMemberOptPerms -> Bool
isTeamOwner tm = optionalPermissions tm == Just fullPermissions

-- | Use this to construct the condition expected by 'teamMemberJson', 'teamMemberListJson'
canSeePermsOf :: TeamMember -> TeamMember -> Bool
canSeePermsOf seeer seeee =
  seeer `hasPermission` GetMemberPermissions || seeer == seeee
