-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.GalleyAPIAccess where

import Control.Lens (to, (^.))
import Data.Default (def)
import Data.Id
import Data.Map qualified as Map
import Data.Proxy
import Data.Range
import Data.SOP (hcpure)
import Imports
import Polysemy
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Team.Feature (AllTeamFeatures, FeatureSingleton (..), IsFeatureConfig (..), LockableFeature (..), npProject')
import Wire.API.Team.FeatureFlags
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfoList (..))
import Wire.API.Team.SearchVisibility
import Wire.GalleyAPIAccess

-- | interprets galley by statically returning the values passed
miniGalleyAPIAccess ::
  -- | what to return when calling GetTeamMember
  Map TeamId [TeamMember] ->
  -- | what to return when calling GetAllTeamFeaturesForUser
  AllTeamFeatures ->
  InterpreterFor GalleyAPIAccess r
miniGalleyAPIAccess teams configs = interpret $ \case
  CreateSelfConv _ -> error "CreateSelfConv not implemented in miniGalleyAPIAccess"
  GetConv _ _ -> error "GetConv not implemented in miniGalleyAPIAccess"
  GetTeamConv {} -> error "GetTeamConv not implemented in miniGalleyAPIAccess"
  NewClient _ _ -> error "NewClient not implemented in miniGalleyAPIAccess"
  CheckUserCanJoinTeam _ -> pure Nothing
  AddTeamMember {} -> error "AddTeamMember not implemented in miniGalleyAPIAccess"
  CreateTeam {} -> error "CreateTeam not implemented in miniGalleyAPIAccess"
  GetTeamMember uid tid -> pure $ getTeamMemberImpl teams uid tid
  GetTeamMembersWithLimit tid maxResults -> pure $ getTeamMembersImpl teams tid maxResults
  GetTeamId _ -> error "GetTeamId not implemented in miniGalleyAPIAccess"
  GetTeam _ -> error "GetTeam not implemented in miniGalleyAPIAccess"
  GetTeamName _ -> error "GetTeamName not implemented in miniGalleyAPIAccess"
  GetTeamLegalHoldStatus _ -> error "GetTeamLegalHoldStatus not implemented in miniGalleyAPIAccess"
  GetUserLegalholdStatus _ _ -> error "GetUserLegalholdStatus not implemented in miniGalleyAPIAccess"
  GetTeamSearchVisibility _ ->
    pure SearchVisibilityStandard
  ChangeTeamStatus {} -> error "ChangeTeamStatus not implemented in miniGalleyAPIAccess"
  FinalizeDeleteTeam {} -> error "FinalizeDeleteTeam not implemented in miniGalleyAPIAccess"
  MemberIsTeamOwner tid uid ->
    pure $ memberIsTeamOwnerImpl teams tid uid
  GetAllTeamFeaturesForUser _ -> pure configs
  GetFeatureConfigForTeam tid -> pure $ getFeatureConfigForTeamImpl configs tid
  GetConfiguredFeatureFlags ->
    pure $ hcpure (Proxy @IsFeatureConfig) mkDefaultFeatureDefaults
  GetVerificationCodeEnabled _ -> error "GetVerificationCodeEnabled not implemented in miniGalleyAPIAccess"
  GetExposeInvitationURLsToTeamAdmin _ -> pure ShowInvitationUrl
  IsMLSOne2OneEstablished _ _ -> error "IsMLSOne2OneEstablished not implemented in miniGalleyAPIAccess"
  UnblockConversation {} -> error "UnblockConversation not implemented in miniGalleyAPIAccess"
  GetEJPDConvInfo _ -> error "GetEJPDConvInfo not implemented in miniGalleyAPIAccess"
  GetTeamAdmins tid -> pure $ newTeamMemberList (maybe [] (filter (\tm -> isAdminOrOwner (tm ^. permissions))) $ Map.lookup tid teams) ListComplete
  SelectTeamMemberInfos tid uids -> pure $ selectTeamMemberInfosImpl teams tid uids
  InternalGetConversation _ -> error "GetConv not implemented in InternalGetConversation"
  GetTeamContacts _ -> pure Nothing
  SelectTeamMembers {} -> error "SelectTeamMembers not implemented in miniGalleyAPIAccess"
  GetConversationConfig ->
    pure
      ConversationSubsystemConfig
        { mlsKeys = Nothing,
          federationProtocols = Nothing,
          legalholdDefaults = FeatureLegalHoldDisabledPermanently,
          maxConvSize = 500
        }

-- this is called but the result is not needed in unit tests
selectTeamMemberInfosImpl :: Map TeamId [TeamMember] -> TeamId -> [UserId] -> TeamMemberInfoList
selectTeamMemberInfosImpl _ _ _ = TeamMemberInfoList []

getFeatureConfigForTeamImpl :: forall feature. (IsFeatureConfig feature) => AllTeamFeatures -> TeamId -> LockableFeature feature
getFeatureConfigForTeamImpl allfeatures _ = npProject' (Proxy @(feature)) allfeatures

memberIsTeamOwnerImpl :: Map TeamId [TeamMember] -> TeamId -> UserId -> Bool
memberIsTeamOwnerImpl teams tid uid =
  case getTeamMemberImpl teams uid tid of
    Nothing -> False
    Just mem -> mem ^. userId == uid && mem ^. permissions . to isAdminOrOwner

getTeamMemberImpl :: Map TeamId [TeamMember] -> UserId -> TeamId -> Maybe TeamMember
getTeamMemberImpl teams uid tid = do
  allMembers <- Map.lookup tid teams
  find (\m -> m ^. userId == uid) allMembers

getTeamMembersImpl :: Map TeamId [TeamMember] -> TeamId -> Maybe (Range 1 HardTruncationLimit Int32) -> TeamMemberList
getTeamMembersImpl teams tid maxResults =
  maybe
    (error "GetTeamMembers not-found case not implemented")
    (newTeamMemberListWithMaxResults (maybe hardTruncationLimit (fromIntegral . fromRange) maxResults))
    $ Map.lookup tid teams

newTeamMemberListWithMaxResults :: Int -> [TeamMember] -> TeamMemberList
newTeamMemberListWithMaxResults maxResults members =
  if length members > maxResults
    then newTeamMemberList (take maxResults members) ListTruncated
    else newTeamMemberList members ListComplete

-- | Helper to create a default FeatureDefaults value for any feature type
-- This is a mock implementation that provides reasonable defaults
mkDefaultFeatureDefaults :: forall cfg. (IsFeatureConfig cfg) => FeatureDefaults cfg
mkDefaultFeatureDefaults = case featureSingleton @cfg of
  FeatureSingletonGuestLinksConfig -> def
  FeatureSingletonLegalholdConfig -> FeatureLegalHoldDisabledByDefault
  FeatureSingletonSSOConfig -> FeatureSSOEnabledByDefault
  FeatureSingletonSearchVisibilityAvailableConfig -> FeatureTeamSearchVisibilityAvailableByDefault
  FeatureSingletonValidateSAMLEmailsConfig -> def
  FeatureSingletonDigitalSignaturesConfig -> DigitalSignaturesDefaults
  FeatureSingletonConferenceCallingConfig -> def
  FeatureSingletonSndFactorPasswordChallengeConfig -> def
  FeatureSingletonSearchVisibilityInboundConfig -> def
  FeatureSingletonClassifiedDomainsConfig -> def
  FeatureSingletonAppLockConfig -> def
  FeatureSingletonSelfDeletingMessagesConfig -> def
  FeatureSingletonFileSharingConfig -> def
  FeatureSingletonMLSConfig -> def
  FeatureSingletonExposeInvitationURLsToTeamAdminConfig -> ExposeInvitationURLsToTeamAdminDefaults
  FeatureSingletonOutlookCalIntegrationConfig -> def
  FeatureSingletonMlsE2EIdConfig -> def
  FeatureSingletonMlsMigrationConfig -> def
  FeatureSingletonEnforceFileDownloadLocationConfig -> def
  FeatureSingletonLimitedEventFanoutConfig -> def
  FeatureSingletonDomainRegistrationConfig -> def
  FeatureSingletonChannelsConfig -> def
  FeatureSingletonCellsConfig -> def
  FeatureSingletonAllowedGlobalOperationsConfig -> def
  FeatureSingletonConsumableNotificationsConfig -> def
  FeatureSingletonChatBubblesConfig -> def
  FeatureSingletonAppsConfig -> def
  FeatureSingletonSimplifiedUserConnectionRequestQRCodeConfig -> def
  FeatureSingletonAssetAuditLogConfig -> def
  FeatureSingletonStealthUsersConfig -> def
  FeatureSingletonCellsInternalConfig -> def
  FeatureSingletonMeetingsConfig -> def
  FeatureSingletonMeetingsPremiumConfig -> def
