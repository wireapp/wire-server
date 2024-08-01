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
  ( getFeatureStatus,
    getFeatureStatusMulti,
    setFeatureStatus,
    setFeatureStatusInternal,
    patchFeatureStatusInternal,
    getAllFeatureConfigsForTeam,
    getAllFeatureConfigsForUser,
    updateLockStatus,
    -- Don't export methods of this typeclass
    GetFeatureConfig,
    -- Don't export methods of this typeclass
    SetFeatureConfig,
    guardSecondFactorDisabled,
    DoAuth (..),
    featureEnabledForTeam,
    guardMlsE2EIdConfig,
  )
where

import Control.Lens
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.UTF8 qualified as UTF8
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.Qualified (Local)
import Data.Schema
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Galley.API.Error (InternalError)
import Galley.API.LegalHold qualified as LegalHold
import Galley.API.Teams (ensureNotTooLargeToActivateLegalHold)
import Galley.API.Teams.Features.Get
import Galley.API.Util (assertTeamExists, getTeamMembersForFanout, membersToRecipients, permissionCheck)
import Galley.App
import Galley.Effects
import Galley.Effects.BrigAccess (updateSearchVisibilityInbound)
import Galley.Effects.SearchVisibilityStore qualified as SearchVisibilityData
import Galley.Effects.TeamFeatureStore
import Galley.Effects.TeamFeatureStore qualified as TeamFeatures
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
import Wire.API.Event.FeatureConfig qualified as Event
import Wire.API.Federation.Error
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.NotificationSubsystem
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

patchFeatureStatusInternal ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetConfigForTeamConstraints cfg r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member (P.Logger (Log.Msg -> Log.Msg)) r,
    Member NotificationSubsystem r
  ) =>
  TeamId ->
  LockableFeaturePatch cfg ->
  Sem r (LockableFeature cfg)
patchFeatureStatusInternal tid patch = do
  assertTeamExists tid
  currentFeatureStatus <- getFeatureStatus @cfg DontDoAuth tid
  let newFeatureStatus = applyPatch currentFeatureStatus
  -- setting the config can fail, so we need to do it first
  void $ setConfigForTeam @cfg tid (forgetLock newFeatureStatus)
  when (isJust $ wspLockStatus patch) $ void $ updateLockStatus @cfg tid (wsLockStatus newFeatureStatus)
  getFeatureStatus @cfg DontDoAuth tid
  where
    applyPatch :: LockableFeature cfg -> LockableFeature cfg
    applyPatch current =
      current
        & setStatus (fromMaybe (wsStatus current) (wspStatus patch))
        & setLockStatus (fromMaybe (wsLockStatus current) (wspLockStatus patch))
        & setConfig (fromMaybe (wsConfig current) (wspConfig patch))
        & setWsTTL (fromMaybe (wsTTL current) (wspTTL patch))

setFeatureStatus ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetConfigForTeamConstraints cfg r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member (P.Logger (Log.Msg -> Log.Msg)) r,
    Member NotificationSubsystem r
  ) =>
  DoAuth ->
  TeamId ->
  Feature cfg ->
  Sem r (LockableFeature cfg)
setFeatureStatus doauth tid wsnl = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ChangeTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  guardLockStatus . wsLockStatus =<< getConfigForTeam @cfg tid
  setConfigForTeam @cfg tid wsnl

setFeatureStatusInternal ::
  forall cfg r.
  ( SetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    SetConfigForTeamConstraints cfg r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
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
setFeatureStatusInternal = setFeatureStatus @cfg DontDoAuth

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
  TeamFeatures.setFeatureLockStatus (featureSingleton @cfg) tid lockStatus
  pure $ LockStatusResponse lockStatus

persistAndPushEvent ::
  forall cfg r.
  ( KnownSymbol (FeatureSymbol cfg),
    ToSchema cfg,
    GetFeatureConfig cfg,
    ComputeFeatureConstraints cfg r,
    Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (P.Logger (Log.Msg -> Log.Msg)) r,
    Member NotificationSubsystem r,
    Member TeamStore r
  ) =>
  TeamId ->
  Feature cfg ->
  Sem r (LockableFeature cfg)
persistAndPushEvent tid wsnl = do
  setFeatureConfig (featureSingleton @cfg) tid wsnl
  fs <- getConfigForTeam @cfg tid
  pushFeatureConfigEvent tid (Event.mkUpdateEvent fs)
  pure fs

pushFeatureConfigEvent ::
  ( Member NotificationSubsystem r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  TeamId ->
  Event.Event ->
  Sem r ()
pushFeatureConfigEvent tid event = do
  memList <- getTeamMembersForFanout tid
  if ((memList ^. teamMemberListType) == ListTruncated)
    then do
      P.warn $
        Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
          . Log.field "feature" (Log.val (toByteString' . Event._eventFeatureName $ event))
          . Log.field "team" (Log.val (UTF8.fromString . show $ tid))
          . Log.msg @Text "Fanout limit exceeded. Events will not be sent."
    else do
      let recipients = membersToRecipients Nothing (memList ^. teamMembers)
      pushNotifications $
        maybeToList $
          (newPush Nothing (toJSONObject event) recipients)

guardLockStatus ::
  forall r.
  (Member (Error TeamFeatureError) r) =>
  LockStatus ->
  Sem r ()
guardLockStatus = \case
  LockStatusUnlocked -> pure ()
  LockStatusLocked -> throw FeatureLocked

-------------------------------------------------------------------------------
-- SetFeatureConfig instances

-- | Don't export methods of this typeclass
class (GetFeatureConfig cfg) => SetFeatureConfig cfg where
  type SetConfigForTeamConstraints cfg (r :: EffectRow) :: Constraint
  type SetConfigForTeamConstraints cfg (r :: EffectRow) = ()

  -- | This method should generate the side-effects of changing the feature and
  -- also (depending on the feature) persist the new setting to the database and
  -- push a event to clients (see 'persistAndPushEvent').
  setConfigForTeam ::
    ( SetConfigForTeamConstraints cfg r,
      ComputeFeatureConstraints cfg r,
      Member (Input Opts) r,
      Member TeamFeatureStore r,
      Member (P.Logger (Log.Msg -> Log.Msg)) r,
      Member NotificationSubsystem r,
      Member TeamStore r
    ) =>
    TeamId ->
    Feature cfg ->
    Sem r (LockableFeature cfg)
  default setConfigForTeam ::
    ( ComputeFeatureConstraints cfg r,
      KnownSymbol (FeatureSymbol cfg),
      ToSchema cfg,
      Member (Input Opts) r,
      Member TeamFeatureStore r,
      Member (P.Logger (Log.Msg -> Log.Msg)) r,
      Member NotificationSubsystem r,
      Member TeamStore r
    ) =>
    TeamId ->
    Feature cfg ->
    Sem r (LockableFeature cfg)
  setConfigForTeam tid wsnl = persistAndPushEvent tid wsnl

instance SetFeatureConfig SSOConfig where
  type
    SetConfigForTeamConstraints SSOConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member (Error TeamFeatureError) r
      )

  setConfigForTeam tid wsnl = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> throw DisableSsoNotImplemented
    persistAndPushEvent tid wsnl

instance SetFeatureConfig SearchVisibilityAvailableConfig where
  type
    SetConfigForTeamConstraints SearchVisibilityAvailableConfig (r :: EffectRow) =
      ( Member SearchVisibilityStore r,
        Member (Input Opts) r
      )

  setConfigForTeam tid wsnl = do
    case wssStatus wsnl of
      FeatureStatusEnabled -> pure ()
      FeatureStatusDisabled -> SearchVisibilityData.resetSearchVisibility tid
    persistAndPushEvent tid wsnl

instance SetFeatureConfig ValidateSAMLEmailsConfig

instance SetFeatureConfig DigitalSignaturesConfig

instance SetFeatureConfig LegalholdConfig where
  type
    SetConfigForTeamConstraints LegalholdConfig (r :: EffectRow) =
      ( Bounded (PagingBounds InternalPaging TeamMember),
        Member BackendNotificationQueueAccess r,
        Member BotAccess r,
        Member BrigAccess r,
        Member CodeStore r,
        Member ConversationStore r,
        Member (Error AuthenticationError) r,
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
        Member (Input UTCTime) r,
        Member LegalHoldStore r,
        Member (ListItems LegacyPaging ConvId) r,
        Member MemberStore r,
        Member ProposalStore r,
        Member SubConversationStore r,
        Member TeamFeatureStore r,
        Member TeamStore r,
        Member (TeamMemberStore InternalPaging) r,
        Member P.TinyLog r,
        Member Random r
      )

  -- we're good to update the status now.
  setConfigForTeam tid wsnl = do
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

    case wssStatus wsnl of
      FeatureStatusDisabled -> LegalHold.removeSettings' @InternalPaging tid
      FeatureStatusEnabled -> ensureNotTooLargeToActivateLegalHold tid
    persistAndPushEvent tid wsnl

instance SetFeatureConfig FileSharingConfig

instance SetFeatureConfig AppLockConfig where
  type SetConfigForTeamConstraints AppLockConfig r = Member (Error TeamFeatureError) r

  setConfigForTeam tid wsnl = do
    when ((applockInactivityTimeoutSecs . wssConfig $ wsnl) < 30) $
      throw AppLockInactivityTimeoutTooLow
    persistAndPushEvent tid wsnl

instance SetFeatureConfig ConferenceCallingConfig

instance SetFeatureConfig SelfDeletingMessagesConfig

instance SetFeatureConfig GuestLinksConfig

instance SetFeatureConfig SndFactorPasswordChallengeConfig

instance SetFeatureConfig SearchVisibilityInboundConfig where
  type SetConfigForTeamConstraints SearchVisibilityInboundConfig (r :: EffectRow) = (Member BrigAccess r)
  setConfigForTeam tid wsnl = do
    updateSearchVisibilityInbound $ toTeamStatus tid wsnl
    persistAndPushEvent tid wsnl

instance SetFeatureConfig MLSConfig where
  type SetConfigForTeamConstraints MLSConfig (r :: EffectRow) = (Member (Error TeamFeatureError) r)
  setConfigForTeam tid wsnl = do
    mlsMigrationConfig <- getConfigForTeam @MlsMigrationConfig tid
    unless
      ( -- default protocol needs to be included in supported protocols
        mlsDefaultProtocol (wssConfig wsnl) `elem` mlsSupportedProtocols (wssConfig wsnl)
          -- when MLS migration is enabled, MLS needs to be enabled as well
          && (wsStatus mlsMigrationConfig == FeatureStatusDisabled || wssStatus wsnl == FeatureStatusEnabled)
      )
      $ throw MLSProtocolMismatch
    persistAndPushEvent tid wsnl

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
guardMlsE2EIdConfig handler uid tid conf = do
  when (isNothing . crlProxy . wssConfig $ conf) $ throw MLSE2EIDMissingCrlProxy
  handler uid tid conf

instance SetFeatureConfig MlsMigrationConfig where
  type SetConfigForTeamConstraints MlsMigrationConfig (r :: EffectRow) = (Member (Error TeamFeatureError) r)
  setConfigForTeam tid wsnl = do
    mlsConfig <- getConfigForTeam @MLSConfig tid
    unless
      ( -- when MLS migration is enabled, MLS needs to be enabled as well
        wssStatus wsnl == FeatureStatusDisabled || wsStatus mlsConfig == FeatureStatusEnabled
      )
      $ throw MLSProtocolMismatch
    persistAndPushEvent tid wsnl

instance SetFeatureConfig EnforceFileDownloadLocationConfig

instance SetFeatureConfig LimitedEventFanoutConfig
