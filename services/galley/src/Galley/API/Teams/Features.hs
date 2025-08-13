{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Galley.API.Teams.Features
  ( setFeature,
    setFeatureInternal,
    patchFeatureInternal,
    getAllTeamFeaturesForTeam,
    getAllTeamFeaturesForUser,
    updateLockStatus,
    GetFeatureConfig (..),
    SetFeatureConfig (..),
    guardSecondFactorDisabled,
    featureEnabledForTeam,
    guardMlsE2EIdConfig,
  )
where

import Control.Lens
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.UTF8 qualified as UTF8
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.Qualified (Local)
import Galley.API.Error (InternalError)
import Galley.API.LegalHold qualified as LegalHold
import Galley.API.LegalHold.Team qualified as LegalHold
import Galley.API.Teams.Features.Get
import Galley.API.Util (assertTeamExists, getTeamMembersForFanout, membersToRecipients, permissionCheck)
import Galley.App
import Galley.Effects
import Galley.Effects.SearchVisibilityStore qualified as SearchVisibilityData
import Galley.Effects.TeamFeatureStore
import Galley.Effects.TeamStore (getLegalHoldFlag, getTeamMember)
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Conversation.Role (Action (RemoveConversationMember))
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley
import Wire.API.Event.FeatureConfig
import Wire.API.Federation.Error
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.BrigAPIAccess (updateSearchVisibilityInbound)
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra
import Wire.TeamCollaboratorsSubsystem

patchFeatureInternal ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetFeatureForTeamConstraints cfg r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
  ) =>
  TeamId ->
  LockableFeaturePatch cfg ->
  Sem r (LockableFeature cfg)
patchFeatureInternal tid patch = do
  assertTeamExists tid
  dbFeature <- getDbFeature tid
  defFeature <- getFeatureForServer @cfg
  let dbFeatureWithDefaults = dbFeature.applyDbFeature defFeature
  let patchedFeature = applyPatch dbFeatureWithDefaults
  prepareFeature tid patchedFeature
  patchDbFeature tid patch
  returnedFeature <- getFeatureForTeam @cfg tid
  pushFeatureEvent @cfg tid (mkUpdateEvent tid returnedFeature)
  pure returnedFeature
  where
    applyPatch :: LockableFeature cfg -> LockableFeature cfg
    applyPatch current =
      current
        { status = fromMaybe current.status patch.status,
          lockStatus = fromMaybe current.lockStatus patch.lockStatus,
          config = fromMaybe current.config patch.config
        }

setFeature ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetFeatureForTeamConstraints cfg r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
  ) =>
  UserId ->
  TeamId ->
  Feature cfg ->
  Sem r (LockableFeature cfg)
setFeature uid tid feat = do
  zusrMembership <- getTeamMember tid uid
  void $ permissionCheck ChangeTeamFeature zusrMembership
  setFeatureUnchecked tid feat

setFeatureInternal ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetFeatureForTeamConstraints cfg r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member NotificationSubsystem r
  ) =>
  TeamId ->
  Feature cfg ->
  Sem r (LockableFeature cfg)
setFeatureInternal tid feat = do
  assertTeamExists tid
  setFeatureUnchecked tid feat

setFeatureUnchecked ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetFeatureForTeamConstraints cfg r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member (P.Logger (Log.Msg -> Log.Msg)) r,
    Member NotificationSubsystem r
  ) =>
  TeamId ->
  Feature cfg ->
  Sem r (LockableFeature cfg)
setFeatureUnchecked tid feat = do
  feat0 <- getFeatureForTeam @cfg tid
  guardLockStatus feat0.lockStatus
  setFeatureForTeam @cfg tid (withLockStatus feat0.lockStatus feat)

updateLockStatus ::
  forall cfg r.
  ( IsFeatureConfig cfg,
    Member TeamFeatureStore r,
    Member TeamStore r,
    Member (ErrorS 'TeamNotFound) r
  ) =>
  TeamId ->
  LockStatus ->
  Sem r LockStatusResponse
updateLockStatus tid lockStatus = do
  assertTeamExists tid
  setFeatureLockStatus @cfg tid lockStatus
  pure $ LockStatusResponse lockStatus

persistFeature ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  TeamId ->
  LockableFeature cfg ->
  Sem r (LockableFeature cfg)
persistFeature tid feat = do
  setDbFeature tid feat
  getFeatureForTeam @cfg tid

pushFeatureEvent ::
  forall cfg r.
  ( IsFeatureConfig cfg,
    Member NotificationSubsystem r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  TeamId ->
  Event ->
  Sem r ()
pushFeatureEvent tid event = do
  memList <- getTeamMembersForFanout tid
  if ((memList ^. teamMemberListType) == ListTruncated)
    then do
      P.warn $
        Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
          . Log.field "feature" (Log.val (toByteString' . _eventFeatureName $ event))
          . Log.field "team" (Log.val (UTF8.fromString . show $ tid))
          . Log.msg @Text "Fanout limit exceeded. Events will not be sent."
    else do
      let recipients = membersToRecipients Nothing (memList ^. teamMembers)
      pushNotifications
        [def {json = toJSONObject event, recipients, isCellsEvent = isCellsFeatureConfigEvent @cfg}]

guardLockStatus ::
  forall r.
  (Member (Error TeamFeatureError) r) =>
  LockStatus ->
  Sem r ()
guardLockStatus = \case
  LockStatusUnlocked -> pure ()
  LockStatusLocked -> throw FeatureLocked

setFeatureForTeam ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    SetFeatureForTeamConstraints cfg r,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member P.TinyLog r,
    Member NotificationSubsystem r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  TeamId ->
  LockableFeature cfg ->
  Sem r (LockableFeature cfg)
setFeatureForTeam tid feat = do
  prepareFeature tid feat
  newFeat <- persistFeature tid feat
  pushFeatureEvent @cfg tid (mkUpdateEvent tid newFeat)
  pure newFeat

-------------------------------------------------------------------------------
-- SetFeatureConfig instances

-- | Don't export methods of this typeclass
class (GetFeatureConfig cfg) => SetFeatureConfig cfg where
  type SetFeatureForTeamConstraints cfg (r :: EffectRow) :: Constraint
  type SetFeatureForTeamConstraints cfg (r :: EffectRow) = ()

  -- | This method takes a feature about to be set, performs the required
  -- checks, makes any related updates via the internal API, then finally
  -- returns the feature to be persisted and pushed to clients.
  --
  -- The default simply returns the original feature unchanged, which should be
  -- enough for most features.
  prepareFeature ::
    (SetFeatureForTeamConstraints cfg r) =>
    TeamId ->
    LockableFeature cfg ->
    Sem r ()
  default prepareFeature :: TeamId -> LockableFeature cfg -> Sem r ()
  prepareFeature _tid _feat = pure ()

instance SetFeatureConfig SSOConfig where
  type
    SetFeatureForTeamConstraints SSOConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member (Error TeamFeatureError) r
      )

  prepareFeature _tid feat = do
    case feat.status of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> throw DisableSsoNotImplemented

instance SetFeatureConfig SearchVisibilityAvailableConfig where
  type
    SetFeatureForTeamConstraints SearchVisibilityAvailableConfig (r :: EffectRow) =
      ( Member SearchVisibilityStore r,
        Member (Input Opts) r
      )

  prepareFeature tid feat = do
    case feat.status of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> SearchVisibilityData.resetSearchVisibility tid

instance SetFeatureConfig ValidateSAMLEmailsConfig

instance SetFeatureConfig DigitalSignaturesConfig

instance SetFeatureConfig LegalholdConfig where
  type
    SetFeatureForTeamConstraints LegalholdConfig (r :: EffectRow) =
      ( Bounded (PagingBounds InternalPaging TeamMember),
        Member BackendNotificationQueueAccess r,
        Member BrigAPIAccess r,
        Member CodeStore r,
        Member ConversationStore r,
        Member (Error FederationError) r,
        Member (Error InternalError) r,
        Member (ErrorS ('ActionDenied 'RemoveConversationMember)) r,
        Member (ErrorS 'CannotEnableLegalHoldServiceLargeTeam) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (Error TeamFeatureError) r,
        Member (ErrorS 'LegalHoldNotEnabled) r,
        Member (ErrorS 'LegalHoldDisableUnimplemented) r,
        Member (ErrorS 'LegalHoldServiceNotRegistered) r,
        Member (ErrorS 'UserLegalHoldIllegalOperation) r,
        Member (ErrorS 'LegalHoldCouldNotBlockConnections) r,
        Member ExternalAccess r,
        Member FederatorAccess r,
        Member FireAndForget r,
        Member NotificationSubsystem r,
        Member (Input (Local ())) r,
        Member (Input Env) r,
        Member Now r,
        Member LegalHoldStore r,
        Member (ListItems LegacyPaging ConvId) r,
        Member MemberStore r,
        Member ProposalStore r,
        Member SubConversationStore r,
        Member TeamFeatureStore r,
        Member TeamStore r,
        Member (TeamMemberStore InternalPaging) r,
        Member P.TinyLog r,
        Member Random r,
        Member (Embed IO) r,
        Member TeamCollaboratorsSubsystem r
      )

  prepareFeature tid feat = do
    -- this extra do is to encapsulate the assertions running before the actual operation.
    -- enabling LH for teams is only allowed in normal operation; disabled-permanently and
    -- whitelist-teams have no or their own way to do that, resp.
    featureLegalHold <- getLegalHoldFlag
    case featureLegalHold of
      FeatureLegalHoldDisabledByDefault -> do
        pure ()
      FeatureLegalHoldDisabledPermanently -> do
        throw LegalHoldFeatureFlagNotEnabled
      FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
        throw LegalHoldWhitelistedOnly

    case feat.status of
      FeatureStatusDisabled -> LegalHold.removeSettings' @InternalPaging tid
      FeatureStatusEnabled -> LegalHold.ensureNotTooLargeToActivateLegalHold tid

instance SetFeatureConfig FileSharingConfig

instance SetFeatureConfig AppLockConfig where
  type SetFeatureForTeamConstraints AppLockConfig r = Member (Error TeamFeatureError) r

  prepareFeature _tid feat = do
    when (feat.config.timeout < 30) $
      throw AppLockInactivityTimeoutTooLow

instance SetFeatureConfig ConferenceCallingConfig

instance SetFeatureConfig SelfDeletingMessagesConfig

instance SetFeatureConfig GuestLinksConfig

instance SetFeatureConfig SndFactorPasswordChallengeConfig

instance SetFeatureConfig SearchVisibilityInboundConfig where
  type SetFeatureForTeamConstraints SearchVisibilityInboundConfig (r :: EffectRow) = (Member BrigAPIAccess r)
  prepareFeature tid feat = do
    updateSearchVisibilityInbound $ toTeamStatus tid feat

instance SetFeatureConfig MLSConfig where
  type
    SetFeatureForTeamConstraints MLSConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member TeamFeatureStore r,
        Member (Error TeamFeatureError) r
      )
  prepareFeature tid feat = do
    mlsMigrationConfig <- getFeatureForTeam @MlsMigrationConfig tid
    unless
      ( -- default protocol needs to be included in supported protocols
        feat.config.mlsDefaultProtocol `elem` feat.config.mlsSupportedProtocols
          -- when MLS migration is enabled, MLS needs to be enabled as well
          && (mlsMigrationConfig.status == FeatureStatusDisabled || feat.status == FeatureStatusEnabled)
      )
      $ throw MLSProtocolMismatch

instance SetFeatureConfig ChannelsConfig

instance SetFeatureConfig ExposeInvitationURLsToTeamAdminConfig

instance SetFeatureConfig OutlookCalIntegrationConfig

instance SetFeatureConfig MlsE2EIdConfig

guardMlsE2EIdConfig ::
  forall r a.
  (Member (Error TeamFeatureError) r) =>
  (UserId -> TeamId -> Feature MlsE2EIdConfig -> Sem r a) ->
  UserId ->
  TeamId ->
  Feature MlsE2EIdConfig ->
  Sem r a
guardMlsE2EIdConfig handler uid tid feat = do
  when (isNothing (getAlt feat.config.crlProxy)) $ throw MLSE2EIDMissingCrlProxy
  handler uid tid feat

instance SetFeatureConfig MlsMigrationConfig where
  type
    SetFeatureForTeamConstraints MlsMigrationConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member (Error TeamFeatureError) r,
        Member TeamFeatureStore r
      )
  prepareFeature tid feat = do
    mlsConfig <- getFeatureForTeam @MLSConfig tid
    unless
      ( -- when MLS migration is enabled, MLS needs to be enabled as well
        feat.status == FeatureStatusDisabled || mlsConfig.status == FeatureStatusEnabled
      )
      $ throw MLSProtocolMismatch

instance SetFeatureConfig EnforceFileDownloadLocationConfig where
  type
    SetFeatureForTeamConstraints EnforceFileDownloadLocationConfig r =
      (Member (Error TeamFeatureError) r)

  prepareFeature _ feat = do
    -- empty download location is not allowed
    -- this is consistent with all other features, and least surprising for clients
    when (feat.config.enforcedDownloadLocation == Just "") $ do
      throw EmptyDownloadLocation

instance SetFeatureConfig LimitedEventFanoutConfig

instance SetFeatureConfig DomainRegistrationConfig

instance SetFeatureConfig CellsConfig

instance SetFeatureConfig ConsumableNotificationsConfig

instance SetFeatureConfig ChatBubblesConfig
