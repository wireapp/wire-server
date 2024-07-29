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
  )
where

import Control.Lens (makeLenses, view)
import Data.Aeson
import Data.Aeson.Types qualified as A
import Data.ByteString (toStrict)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Id (UserId)
import Data.Schema qualified as Schema
import Data.Set qualified as Set
import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission

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
    _flagConferenceCalling :: !(Defaults (ImplicitLockStatus ConferenceCallingConfig)),
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

instance (FromJSON a) => FromJSON (Defaults a) where
  parseJSON = withObject "default object" $ \ob ->
    Defaults <$> (ob .: "defaults")

instance (ToJSON a) => ToJSON (Defaults a) where
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
      <*> withImplicitLockStatusOrDefault obj "conferenceCalling"
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

instance ToJSON FeatureFlags where
  toJSON
    ( FeatureFlags
        sso
        legalhold
        searchVisibility
        appLock
        classifiedDomains
        fileSharing
        conferenceCalling
        selfDeletingMessages
        guestLinks
        validateSAMLEmails
        sndFactorPasswordChallenge
        searchVisibilityInbound
        mls
        outlookCalIntegration
        mlsE2EId
        mlsMigration
        enforceFileDownloadLocation
        teamMemberDeletedLimitedEventFanout
      ) =
      object
        [ "sso" .= sso,
          "legalhold" .= legalhold,
          "teamSearchVisibility" .= searchVisibility,
          "appLock" .= appLock,
          "classifiedDomains" .= classifiedDomains,
          "fileSharing" .= fileSharing,
          "conferenceCalling" .= conferenceCalling,
          "selfDeletingMessages" .= selfDeletingMessages,
          "conversationGuestLinks" .= guestLinks,
          "validateSAMLEmails" .= validateSAMLEmails,
          "sndFactorPasswordChallenge" .= sndFactorPasswordChallenge,
          "searchVisibilityInbound" .= searchVisibilityInbound,
          "mls" .= mls,
          "outlookCalIntegration" .= outlookCalIntegration,
          "mlsE2EId" .= mlsE2EId,
          "mlsMigration" .= mlsMigration,
          "enforceFileDownloadLocation" .= enforceFileDownloadLocation,
          "limitedEventFanout" .= teamMemberDeletedLimitedEventFanout
        ]

instance FromJSON FeatureSSO where
  parseJSON (String "enabled-by-default") = pure FeatureSSOEnabledByDefault
  parseJSON (String "disabled-by-default") = pure FeatureSSODisabledByDefault
  parseJSON bad = fail $ "FeatureSSO: " <> (UTF8.toString . toStrict . encode $ bad)

instance ToJSON FeatureSSO where
  toJSON FeatureSSOEnabledByDefault = String "enabled-by-default"
  toJSON FeatureSSODisabledByDefault = String "disabled-by-default"

instance FromJSON FeatureLegalHold where
  parseJSON (String "disabled-permanently") = pure $ FeatureLegalHoldDisabledPermanently
  parseJSON (String "disabled-by-default") = pure $ FeatureLegalHoldDisabledByDefault
  parseJSON (String "whitelist-teams-and-implicit-consent") = pure FeatureLegalHoldWhitelistTeamsAndImplicitConsent
  parseJSON bad = fail $ "FeatureLegalHold: " <> (UTF8.toString . toStrict . encode $ bad)

instance ToJSON FeatureLegalHold where
  toJSON FeatureLegalHoldDisabledPermanently = String "disabled-permanently"
  toJSON FeatureLegalHoldDisabledByDefault = String "disabled-by-default"
  toJSON FeatureLegalHoldWhitelistTeamsAndImplicitConsent = String "whitelist-teams-and-implicit-consent"

instance FromJSON FeatureTeamSearchVisibilityAvailability where
  parseJSON (String "enabled-by-default") = pure FeatureTeamSearchVisibilityAvailableByDefault
  parseJSON (String "disabled-by-default") = pure FeatureTeamSearchVisibilityUnavailableByDefault
  parseJSON bad = fail $ "FeatureSearchVisibility: " <> (UTF8.toString . toStrict . encode $ bad)

instance ToJSON FeatureTeamSearchVisibilityAvailability where
  toJSON FeatureTeamSearchVisibilityAvailableByDefault = String "enabled-by-default"
  toJSON FeatureTeamSearchVisibilityUnavailableByDefault = String "disabled-by-default"

makeLenses ''TeamCreationTime
makeLenses ''FeatureFlags
makeLenses ''Defaults

notTeamMember :: [UserId] -> [TeamMember] -> [UserId]
notTeamMember uids tmms =
  Set.toList $
    Set.fromList uids `Set.difference` Set.fromList (map (view userId) tmms)

isTeamMember :: (Foldable m) => UserId -> m TeamMember -> Bool
isTeamMember u = isJust . findTeamMember u

findTeamMember :: (Foldable m) => UserId -> m TeamMember -> Maybe TeamMember
findTeamMember u = find ((u ==) . view userId)

isTeamOwner :: TeamMemberOptPerms -> Bool
isTeamOwner tm = optionalPermissions tm == Just fullPermissions

canSeePermsOf :: TeamMember -> TeamMember -> Bool
canSeePermsOf seeer seeee =
  seeer `hasPermission` GetMemberPermissions || seeer == seeee
