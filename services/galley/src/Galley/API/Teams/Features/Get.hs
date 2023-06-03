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

module Galley.API.Teams.Features.Get
  ( getFeatureStatus,
    getFeatureStatusMulti,
    getFeatureStatusForUser,
    getAllFeatureConfigsForServer,
    getAllFeatureConfigsForTeam,
    getAllFeatureConfigsForUser,
    GetFeatureConfig (..),
    guardSecondFactorDisabled,
    DoAuth (..),
    featureEnabledForTeam,
    toTeamStatus,
  )
where

import Control.Lens
import Data.Bifunctor (second)
import Data.Id
import Data.Kind
import Data.Qualified (Local, tUnqualified)
import Galley.API.LegalHold.Team (isLegalHoldEnabledForTeam)
import Galley.API.Util
import Galley.Effects
import Galley.Effects.BrigAccess (getAccountConferenceCallingConfigClient)
import Galley.Effects.ConversationStore as ConversationStore
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore (getOneUserTeam, getTeam, getTeamMember)
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Conversation (cnvmTeam)
import Wire.API.Error (ErrorS, throwS)
import Wire.API.Error.Galley
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature

data DoAuth = DoAuth UserId | DontDoAuth

-- | Don't export methods of this typeclass
class IsFeatureConfig cfg => GetFeatureConfig cfg where
  type GetConfigForTeamConstraints cfg (r :: EffectRow) :: Constraint
  type
    GetConfigForTeamConstraints cfg (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member TeamFeatureStore r
      )

  type GetConfigForUserConstraints cfg (r :: EffectRow) :: Constraint
  type
    GetConfigForUserConstraints cfg (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r,
        Member TeamStore r,
        Member TeamFeatureStore r
      )

  getConfigForServer ::
    Member (Input Opts) r =>
    Sem r (WithStatus cfg)
  -- only override if there is additional business logic for getting the feature config
  -- and/or if the feature flag is configured for the backend in 'FeatureFlags' for galley in 'Galley.Types.Teams'
  -- otherwise this will return the default config from wire-api
  default getConfigForServer :: Sem r (WithStatus cfg)
  getConfigForServer = pure defFeatureStatus

  getConfigForTeam ::
    GetConfigForTeamConstraints cfg r =>
    TeamId ->
    Sem r (WithStatus cfg)
  default getConfigForTeam ::
    ( Member (Input Opts) r,
      Member TeamFeatureStore r
    ) =>
    TeamId ->
    Sem r (WithStatus cfg)
  getConfigForTeam = genericGetConfigForTeam

  getConfigForUser ::
    GetConfigForUserConstraints cfg r =>
    UserId ->
    Sem r (WithStatus cfg)
  default getConfigForUser ::
    ( Member (Input Opts) r,
      Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS 'TeamNotFound) r,
      Member TeamStore r,
      Member TeamFeatureStore r
    ) =>
    UserId ->
    Sem r (WithStatus cfg)
  getConfigForUser = genericGetConfigForUser

getFeatureStatus ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    GetConfigForTeamConstraints cfg r,
    ( Member (ErrorS OperationDenied) r,
      Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS 'TeamNotFound) r,
      Member TeamStore r
    )
  ) =>
  DoAuth ->
  TeamId ->
  Sem r (WithStatus cfg)
getFeatureStatus doauth tid = do
  case doauth of
    DoAuth uid ->
      getTeamMember tid uid >>= maybe (throwS @'NotATeamMember) (const $ pure ())
    DontDoAuth ->
      assertTeamExists tid
  getConfigForTeam @cfg tid

getFeatureStatusMulti ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  Sem r (Multi.TeamFeatureNoConfigMultiResponse cfg)
getFeatureStatusMulti (Multi.TeamFeatureNoConfigMultiRequest tids) = do
  cfgs <- genericGetConfigForMultiTeam @cfg tids
  let xs = uncurry toTeamStatus . second forgetLock <$> cfgs
  pure $ Multi.TeamFeatureNoConfigMultiResponse xs

toTeamStatus :: TeamId -> WithStatusNoLock cfg -> Multi.TeamStatus cfg
toTeamStatus tid ws = Multi.TeamStatus tid (wssStatus ws)

-- | For individual users to get feature config for their account (personal or team).
-- This looks supposedly redundant to the implementations of `getConfigForUser` but it's not.
-- Here we explicitly return the team setting if the user is a team member.
-- In `getConfigForUser` this is mostly also the case. But there are exceptions, e.g. `ConferenceCallingConfig`
getFeatureStatusForUser ::
  forall cfg r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    GetConfigForTeamConstraints cfg r,
    GetConfigForUserConstraints cfg r,
    GetFeatureConfig cfg
  ) =>
  UserId ->
  Sem r (WithStatus cfg)
getFeatureStatusForUser zusr = do
  mbTeam <- getOneUserTeam zusr
  case mbTeam of
    Nothing ->
      getConfigForUser @cfg zusr
    Just tid -> do
      zusrMembership <- getTeamMember tid zusr
      void $ maybe (throwS @'NotATeamMember) pure zusrMembership
      assertTeamExists tid
      getConfigForTeam @cfg tid

getAllFeatureConfigsForUser ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForUser zusr = do
  mbTeam <- getOneUserTeam zusr
  when (isJust mbTeam) $ do
    zusrMembership <- maybe (pure Nothing) (`getTeamMember` zusr) mbTeam
    maybe (throwS @'NotATeamMember) (const $ pure ()) zusrMembership
  case mbTeam of
    Just tid ->
      getAllFeatureConfigsTeam tid
    Nothing ->
      getAllFeatureConfigsUser zusr

getAllFeatureConfigsForTeam ::
  forall r.
  ( Member (ErrorS 'NotATeamMember) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForTeam luid tid = do
  zusrMembership <- getTeamMember tid (tUnqualified luid)
  maybe (throwS @'NotATeamMember) (const $ pure ()) zusrMembership
  getAllFeatureConfigsTeam tid

getAllFeatureConfigsForServer ::
  forall r.
  Member (Input Opts) r =>
  Sem r AllFeatureConfigs
getAllFeatureConfigsForServer =
  AllFeatureConfigs
    <$> getConfigForServer @LegalholdConfig
    <*> getConfigForServer @SSOConfig
    <*> getConfigForServer @SearchVisibilityAvailableConfig
    <*> getConfigForServer @SearchVisibilityInboundConfig
    <*> getConfigForServer @ValidateSAMLEmailsConfig
    <*> getConfigForServer @DigitalSignaturesConfig
    <*> getConfigForServer @AppLockConfig
    <*> getConfigForServer @FileSharingConfig
    <*> getConfigForServer @ClassifiedDomainsConfig
    <*> getConfigForServer @ConferenceCallingConfig
    <*> getConfigForServer @SelfDeletingMessagesConfig
    <*> getConfigForServer @GuestLinksConfig
    <*> getConfigForServer @SndFactorPasswordChallengeConfig
    <*> getConfigForServer @MLSConfig
    <*> getConfigForServer @ExposeInvitationURLsToTeamAdminConfig
    <*> getConfigForServer @OutlookCalIntegrationConfig
    <*> getConfigForServer @MlsE2EIdConfig

getAllFeatureConfigsUser ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member (ErrorS OperationDenied) r,
    Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsUser uid =
  AllFeatureConfigs
    <$> getConfigForUser @LegalholdConfig uid
    <*> getConfigForUser @SSOConfig uid
    <*> getConfigForUser @SearchVisibilityAvailableConfig uid
    <*> getConfigForUser @SearchVisibilityInboundConfig uid
    <*> getConfigForUser @ValidateSAMLEmailsConfig uid
    <*> getConfigForUser @DigitalSignaturesConfig uid
    <*> getConfigForUser @AppLockConfig uid
    <*> getConfigForUser @FileSharingConfig uid
    <*> getConfigForUser @ClassifiedDomainsConfig uid
    <*> getConfigForUser @ConferenceCallingConfig uid
    <*> getConfigForUser @SelfDeletingMessagesConfig uid
    <*> getConfigForUser @GuestLinksConfig uid
    <*> getConfigForUser @SndFactorPasswordChallengeConfig uid
    <*> getConfigForUser @MLSConfig uid
    <*> getConfigForUser @ExposeInvitationURLsToTeamAdminConfig uid
    <*> getConfigForUser @OutlookCalIntegrationConfig uid
    <*> getConfigForUser @MlsE2EIdConfig uid

getAllFeatureConfigsTeam ::
  forall r.
  ( Member (Input Opts) r,
    Member LegalHoldStore r,
    Member TeamFeatureStore r,
    Member TeamStore r
  ) =>
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsTeam tid =
  AllFeatureConfigs
    <$> getConfigForTeam @LegalholdConfig tid
    <*> getConfigForTeam @SSOConfig tid
    <*> getConfigForTeam @SearchVisibilityAvailableConfig tid
    <*> getConfigForTeam @SearchVisibilityInboundConfig tid
    <*> getConfigForTeam @ValidateSAMLEmailsConfig tid
    <*> getConfigForTeam @DigitalSignaturesConfig tid
    <*> getConfigForTeam @AppLockConfig tid
    <*> getConfigForTeam @FileSharingConfig tid
    <*> getConfigForTeam @ClassifiedDomainsConfig tid
    <*> getConfigForTeam @ConferenceCallingConfig tid
    <*> getConfigForTeam @SelfDeletingMessagesConfig tid
    <*> getConfigForTeam @GuestLinksConfig tid
    <*> getConfigForTeam @SndFactorPasswordChallengeConfig tid
    <*> getConfigForTeam @MLSConfig tid
    <*> getConfigForTeam @ExposeInvitationURLsToTeamAdminConfig tid
    <*> getConfigForTeam @OutlookCalIntegrationConfig tid
    <*> getConfigForTeam @MlsE2EIdConfig tid

-- | Note: this is an internal function which doesn't cover all features, e.g. LegalholdConfig
genericGetConfigForTeam ::
  forall cfg r.
  GetFeatureConfig cfg =>
  Member TeamFeatureStore r =>
  Member (Input Opts) r =>
  TeamId ->
  Sem r (WithStatus cfg)
genericGetConfigForTeam tid = do
  computeFeatureConfigForTeamUser
    <$> TeamFeatures.getFeatureConfig (featureSingleton @cfg) tid
    <*> TeamFeatures.getFeatureLockStatus (featureSingleton @cfg) tid
    <*> getConfigForServer

-- Note: this function assumes the feature cannot be locked
genericGetConfigForMultiTeam ::
  forall cfg r.
  GetFeatureConfig cfg =>
  Member TeamFeatureStore r =>
  Member (Input Opts) r =>
  [TeamId] ->
  Sem r [(TeamId, WithStatus cfg)]
genericGetConfigForMultiTeam tids = do
  def <- getConfigForServer
  (\(tid, mwsnl) -> (tid, computeFeatureConfigForTeamUser mwsnl (Just LockStatusUnlocked) def))
    <$$> TeamFeatures.getFeatureConfigMulti (featureSingleton @cfg) tids

-- | Note: this is an internal function which doesn't cover all features, e.g. conference calling
genericGetConfigForUser ::
  forall cfg r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    GetFeatureConfig cfg
  ) =>
  UserId ->
  Sem r (WithStatus cfg)
genericGetConfigForUser uid = do
  mbTeam <- getOneUserTeam uid
  case mbTeam of
    Nothing -> do
      getConfigForServer
    Just tid -> do
      zusrMembership <- getTeamMember tid uid
      maybe (throwS @'NotATeamMember) (const $ pure ()) zusrMembership
      assertTeamExists tid
      genericGetConfigForTeam tid

-------------------------------------------------------------------------------
-- GetFeatureConfig instances

instance GetFeatureConfig SSOConfig where
  getConfigForServer = do
    status <-
      inputs @Opts (.settings.featureFlags.sso) <&> \case
        FeatureSSOEnabledByDefault -> FeatureStatusEnabled
        FeatureSSODisabledByDefault -> FeatureStatusDisabled
    pure $ setStatus status defFeatureStatus

  getConfigForUser = genericGetConfigForUser

instance GetFeatureConfig SearchVisibilityAvailableConfig where
  getConfigForServer = do
    status <-
      inputs @Opts (.settings.featureFlags.teamSearchVisibility) <&> \case
        FeatureTeamSearchVisibilityAvailableByDefault -> FeatureStatusEnabled
        FeatureTeamSearchVisibilityUnavailableByDefault -> FeatureStatusDisabled
    pure $ setStatus status defFeatureStatus

instance GetFeatureConfig ValidateSAMLEmailsConfig where
  getConfigForServer =
    inputs @Opts (.settings.featureFlags.teamFeatureValidateSAMLEmailsStatus.unDefaults.unImplicitLockStatus)

instance GetFeatureConfig DigitalSignaturesConfig

instance GetFeatureConfig LegalholdConfig where
  type
    GetConfigForTeamConstraints LegalholdConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member TeamFeatureStore r,
        Member LegalHoldStore r,
        Member TeamStore r
      )
  type
    GetConfigForUserConstraints LegalholdConfig (r :: EffectRow) =
      ( Member (Input Opts) r,
        Member TeamFeatureStore r,
        Member LegalHoldStore r,
        Member TeamStore r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r
      )

  getConfigForTeam tid = do
    status <-
      isLegalHoldEnabledForTeam tid <&> \case
        True -> FeatureStatusEnabled
        False -> FeatureStatusDisabled
    pure $ setStatus status defFeatureStatus

instance GetFeatureConfig FileSharingConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.fileSharing.unDefaults)

instance GetFeatureConfig AppLockConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.appLockDefaults.unDefaults.unImplicitLockStatus)

instance GetFeatureConfig ClassifiedDomainsConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.classifiedDomains.unImplicitLockStatus)

instance GetFeatureConfig ConferenceCallingConfig where
  type
    GetConfigForUserConstraints ConferenceCallingConfig r =
      ( Member (Input Opts) r,
        Member (ErrorS OperationDenied) r,
        Member (ErrorS 'NotATeamMember) r,
        Member (ErrorS 'TeamNotFound) r,
        Member TeamStore r,
        Member TeamFeatureStore r,
        Member BrigAccess r
      )

  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.conferenceCalling.unDefaults.unImplicitLockStatus)

  getConfigForUser uid = do
    wsnl <- getAccountConferenceCallingConfigClient uid
    pure $ withLockStatus (wsLockStatus (defFeatureStatus @ConferenceCallingConfig)) wsnl

instance GetFeatureConfig SelfDeletingMessagesConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.selfDeletingMessages.unDefaults)

instance GetFeatureConfig GuestLinksConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.conversationGuestLinks.unDefaults)

instance GetFeatureConfig SndFactorPasswordChallengeConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.teamFeatureSndFactorPasswordChallengeStatus.unDefaults)

instance GetFeatureConfig SearchVisibilityInboundConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.teamFeatureSearchVisibilityInbound.unDefaults.unImplicitLockStatus)

instance GetFeatureConfig MLSConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.mls.unDefaults.unImplicitLockStatus)

instance GetFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  getConfigForTeam tid = do
    allowList <- fromMaybe [] <$> inputs @Opts (.settings.exposeInvitationURLsTeamAllowlist)
    mbOldStatus <- TeamFeatures.getFeatureConfig FeatureSingletonExposeInvitationURLsToTeamAdminConfig tid <&> fmap wssStatus
    let teamAllowed = tid `elem` allowList
    pure $ computeConfigForTeam teamAllowed (fromMaybe FeatureStatusDisabled mbOldStatus)
    where
      computeConfigForTeam :: Bool -> FeatureStatus -> WithStatus ExposeInvitationURLsToTeamAdminConfig
      computeConfigForTeam teamAllowed teamDbStatus =
        if teamAllowed
          then makeConfig LockStatusUnlocked teamDbStatus
          else makeConfig LockStatusLocked FeatureStatusDisabled

      makeConfig :: LockStatus -> FeatureStatus -> WithStatus ExposeInvitationURLsToTeamAdminConfig
      makeConfig lockStatus status =
        withStatus
          status
          lockStatus
          ExposeInvitationURLsToTeamAdminConfig
          FeatureTTLUnlimited

instance GetFeatureConfig OutlookCalIntegrationConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.outlookCalIntegration.unDefaults)

instance GetFeatureConfig MlsE2EIdConfig where
  getConfigForServer =
    input @Opts <&> (.settings.featureFlags.mlsE2EId.unDefaults)

-- -- | If second factor auth is enabled, make sure that end-points that don't support it, but should, are blocked completely.  (This is a workaround until we have 2FA for those end-points as well.)
-- --
-- This function exists to resolve a cyclic dependency.
guardSecondFactorDisabled ::
  forall r a.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'AccessDenied) r,
    Member (ErrorS OperationDenied) r,
    Member (ErrorS 'NotATeamMember) r,
    Member (ErrorS 'TeamNotFound) r,
    Member TeamStore r,
    Member ConversationStore r
  ) =>
  UserId ->
  ConvId ->
  Sem r a ->
  Sem r a
guardSecondFactorDisabled uid cid action = do
  mbCnvData <- ConversationStore.getConversationMetadata cid
  tf <- case mbCnvData >>= cnvmTeam of
    Nothing -> getConfigForUser @SndFactorPasswordChallengeConfig uid
    Just tid -> do
      teamExists <- isJust <$> getTeam tid
      if teamExists
        then getConfigForTeam @SndFactorPasswordChallengeConfig tid
        else getConfigForUser @SndFactorPasswordChallengeConfig uid
  case wsStatus tf of
    FeatureStatusDisabled -> action
    FeatureStatusEnabled -> throwS @'AccessDenied

featureEnabledForTeam ::
  forall cfg r.
  ( GetFeatureConfig cfg,
    GetConfigForTeamConstraints cfg r,
    ( Member (ErrorS OperationDenied) r,
      Member (ErrorS 'NotATeamMember) r,
      Member (ErrorS 'TeamNotFound) r,
      Member TeamStore r
    )
  ) =>
  TeamId ->
  Sem r Bool
featureEnabledForTeam tid = (==) FeatureStatusEnabled . wsStatus <$> getFeatureStatus @cfg DontDoAuth tid
