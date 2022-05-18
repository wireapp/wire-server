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
{-# LANGUAGE RecordWildCards #-}

module Galley.API.Teams.Features
  ( getFeatureStatus,
    getFeatureStatusNoConfig,
    setFeatureStatus,
    getFeatureConfig,
    getAllFeatureConfigsForUser,
    getAllFeatureConfigsForTeam,
    getSSOStatusInternal,
    setSSOStatusInternal,
    getLegalholdStatusInternal,
    setLegalholdStatusInternal,
    getTeamSearchVisibilityAvailableInternal,
    setTeamSearchVisibilityAvailableInternal,
    getValidateSAMLEmailsInternal,
    setValidateSAMLEmailsInternal,
    getDigitalSignaturesInternal,
    setDigitalSignaturesInternal,
    getClassifiedDomainsInternal,
    getAppLockInternal,
    setAppLockInternal,
    getFileSharingInternal,
    setFileSharingInternal,
    getConferenceCallingInternal,
    setConferenceCallingInternal,
    getSelfDeletingMessagesInternal,
    setSelfDeletingMessagesInternal,
    getSndFactorPasswordChallengeInternal,
    getSndFactorPasswordChallengeNoAuth,
    setSndFactorPasswordChallengeInternal,
    getTeamSearchVisibilityInboundInternal,
    setTeamSearchVisibilityInboundInternal,
    getTeamSearchVisibilityInboundInternalMulti,
    getGuestLinkInternal,
    setGuestLinkInternal,
    setLockStatus,
    DoAuth (..),
    FeatureGetter,
    FeatureSetter,
    FeatureScope,
    guardSecondFactorDisabled,
  )
where

import Control.Lens
import Data.ByteString.Conversion hiding (fromList)
import Data.Id
import Data.Proxy (Proxy (Proxy))
import Data.Qualified
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Tagged
import Data.Time.Clock
import Galley.API.Error as Galley
import Galley.API.LegalHold
import Galley.API.Teams (ensureNotTooLargeToActivateLegalHold)
import Galley.API.Util
import Galley.Cassandra.Paging
import Galley.Data.TeamFeatures
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.ConversationStore as ConversationStore
import Galley.Effects.GundeckAccess
import Galley.Effects.Paging
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore
import Galley.Intra.Push (PushEvent (FeatureConfigEvent), newPush)
import Galley.Options
import Galley.Types
import Galley.Types.Teams hiding (newTeam)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import qualified System.Logger.Class as Log
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.FeatureConfig as Event
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature

data DoAuth = DoAuth UserId | DontDoAuth

data FeatureScope
  = FeatureScopeServer
  | FeatureScopeTeam TeamId
  | FeatureScopeUser UserId

type FeatureGetter l f r = Tagged '(l, f) (FeatureScope -> Sem r (TeamFeatureStatus l f))

type FeatureSetter f r =
  Tagged
    f
    ( TeamId ->
      TeamFeatureStatus 'WithoutLockStatus f ->
      Sem r (TeamFeatureStatus 'WithoutLockStatus f)
    )

-- | For team-settings, to administrate team feature configuration.  Here we have an admin uid
-- and a team id, but no uid of the member for which the feature config holds.
getFeatureStatus ::
  forall (ps :: IncludeLockStatus) (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    Members
      '[ ErrorS OperationDenied,
         ErrorS 'NotATeamMember,
         ErrorS 'TeamNotFound,
         TeamStore
       ]
      r
  ) =>
  FeatureGetter ps a r ->
  DoAuth ->
  TeamId ->
  Sem r (TeamFeatureStatus ps a)
getFeatureStatus (Tagged getter) doauth tid = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ViewTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getter (FeatureScopeTeam tid)

-- | For team-settings, like 'getFeatureStatus'.
setFeatureStatus ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    MaybeHasLockStatusCol a,
    Members
      '[ ErrorS 'NotATeamMember,
         ErrorS OperationDenied,
         ErrorS 'TeamNotFound,
         TeamStore,
         TeamFeatureStore
       ]
      r
  ) =>
  FeatureSetter a r ->
  DoAuth ->
  TeamId ->
  TeamFeatureStatus 'WithoutLockStatus a ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
setFeatureStatus (Tagged setter) doauth tid status = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck ChangeTeamFeature zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  setter tid status

-- | Setting lock status can only be done through the internal API and therefore doesn't require auth.
setLockStatus ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    HasLockStatusCol a,
    Members
      [ ErrorS 'NotATeamMember,
        ErrorS 'TeamNotFound,
        TeamStore,
        TeamFeatureStore
      ]
      r
  ) =>
  TeamId ->
  LockStatusValue ->
  Sem r LockStatus
setLockStatus tid lockStatusUpdate = do
  assertTeamExists tid
  TeamFeatures.setLockStatus @a tid (LockStatus lockStatusUpdate)

-- | For individual users to get feature config for their account (personal or team).
getFeatureConfig ::
  forall (ps :: IncludeLockStatus) (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    Members
      '[ ErrorS 'NotATeamMember,
         ErrorS OperationDenied,
         ErrorS 'TeamNotFound,
         TeamStore
       ]
      r
  ) =>
  FeatureGetter ps a r ->
  UserId ->
  Sem r (TeamFeatureStatus ps a)
getFeatureConfig (Tagged getter) zusr = do
  mbTeam <- getOneUserTeam zusr
  case mbTeam of
    Nothing -> getter (FeatureScopeUser zusr)
    Just tid -> do
      zusrMembership <- getTeamMember tid zusr
      void $ permissionCheck ViewTeamFeature zusrMembership
      assertTeamExists tid
      getter (FeatureScopeTeam tid)

-- | Get feature config for a user. If the user is a member of a team and has the required permissions, this will return the team's feature configs.
-- If the user is not a member of a team, this will return the personal feature configs (the server defaults).
getAllFeatureConfigsForUser ::
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore
     ]
    r =>
  UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForUser zusr = do
  mbTeam <- getOneUserTeam zusr
  when (isJust mbTeam) $ do
    zusrMembership <- maybe (pure Nothing) (`getTeamMember` zusr) mbTeam
    void $ permissionCheck ViewTeamFeature zusrMembership
  let scope = maybe (FeatureScopeUser zusr) FeatureScopeTeam mbTeam
  getAllFeatureConfigsInternal scope

-- | Get feature configs for a team. User must be a member of the team and have permission to view team features.
getAllFeatureConfigsForTeam ::
  forall r.
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       ErrorS 'TeamNotFound,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore
     ]
    r =>
  Local UserId ->
  TeamId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForTeam luid tid = do
  zusrMembership <- getTeamMember tid (tUnqualified luid)
  void $ permissionCheck ViewTeamFeature zusrMembership
  getAllFeatureConfigsInternal (FeatureScopeTeam tid)

getAllFeatureConfigsInternal ::
  Members
    '[ BrigAccess,
       ErrorS 'NotATeamMember,
       ErrorS OperationDenied,
       Input Opts,
       LegalHoldStore,
       TeamFeatureStore,
       TeamStore
     ]
    r =>
  FeatureScope ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsInternal byUserOrTeam =
  AllFeatureConfigs
    <$> unTagged getLegalholdStatusInternal byUserOrTeam
    <*> unTagged getSSOStatusInternal byUserOrTeam
    <*> unTagged getTeamSearchVisibilityAvailableInternal byUserOrTeam
    <*> unTagged getValidateSAMLEmailsInternal byUserOrTeam
    <*> unTagged getDigitalSignaturesInternal byUserOrTeam
    <*> unTagged getAppLockInternal byUserOrTeam
    <*> unTagged getFileSharingInternal byUserOrTeam
    <*> unTagged getClassifiedDomainsInternal byUserOrTeam
    <*> unTagged getConferenceCallingInternal byUserOrTeam
    <*> unTagged getSelfDeletingMessagesInternal byUserOrTeam
    <*> unTagged getGuestLinkInternal byUserOrTeam
    <*> unTagged getSndFactorPasswordChallengeInternal byUserOrTeam

getFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  ( FeatureHasNoConfig 'WithoutLockStatus a,
    HasStatusCol a,
    Member TeamFeatureStore r
  ) =>
  Sem r TeamFeatureStatusValue ->
  TeamId ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- TeamFeatureStatusNoConfig <$> getDefault
  fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @a tid

setFeatureStatusNoConfig ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    FeatureHasNoConfig 'WithoutLockStatus a,
    HasStatusCol a,
    Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r
  ) =>
  (TeamFeatureStatusValue -> TeamId -> Sem r ()) ->
  FeatureSetter a r
setFeatureStatusNoConfig applyState = Tagged $ \tid status -> do
  applyState (tfwoStatus status) tid
  newStatus <- TeamFeatures.setFeatureStatusNoConfig @a tid status
  pushFeatureConfigEvent tid $
    Event.Event Event.Update (knownTeamFeatureName @a) (EdFeatureWithoutConfigChanged newStatus)
  pure newStatus

getSSOStatusInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSSO r
getSSOStatusInternal =
  Tagged $ \case
    FeatureScopeTeam tid ->
      getFeatureStatusNoConfig @'TeamFeatureSSO getDef tid
    FeatureScopeUser _ -> TeamFeatureStatusNoConfig <$> getDef
    FeatureScopeServer -> TeamFeatureStatusNoConfig <$> getDef
  where
    getDef :: Member (Input Opts) r => Sem r TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . flagSSO)) <&> \case
        FeatureSSOEnabledByDefault -> TeamFeatureEnabled
        FeatureSSODisabledByDefault -> TeamFeatureDisabled

setSSOStatusInternal ::
  Members '[Error TeamFeatureError, GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureSSO r
setSSOStatusInternal = setFeatureStatusNoConfig @'TeamFeatureSSO $ \case
  TeamFeatureDisabled -> const (throw DisableSsoNotImplemented)
  TeamFeatureEnabled -> const (pure ())

getTeamSearchVisibilityAvailableInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSearchVisibility r
getTeamSearchVisibilityAvailableInternal =
  Tagged $ \case
    FeatureScopeTeam tid -> getFeatureStatusNoConfig @'TeamFeatureSearchVisibility getDef tid
    FeatureScopeUser _ -> TeamFeatureStatusNoConfig <$> getDef
    FeatureScopeServer -> TeamFeatureStatusNoConfig <$> getDef
  where
    getDef = do
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> TeamFeatureEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal ::
  Members '[GundeckAccess, SearchVisibilityStore, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureSearchVisibility r
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'TeamFeatureSearchVisibility $ \case
  TeamFeatureDisabled -> SearchVisibilityData.resetSearchVisibility
  TeamFeatureEnabled -> const (pure ())

getValidateSAMLEmailsInternal ::
  forall r.
  ( Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureValidateSAMLEmails r
getValidateSAMLEmailsInternal =
  Tagged $ getFeatureStatusWithDefaultConfig @'TeamFeatureValidateSAMLEmails flagsTeamFeatureValidateSAMLEmailsStatus . mbTeam
  where
    mbTeam = \case
      FeatureScopeTeam tid -> Just tid
      FeatureScopeUser _ -> Nothing
      FeatureScopeServer -> Nothing

setValidateSAMLEmailsInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureValidateSAMLEmails r
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal ::
  Member TeamFeatureStore r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureDigitalSignatures r
getDigitalSignaturesInternal =
  Tagged $ \case
    FeatureScopeTeam tid -> getFeatureStatusNoConfig @'TeamFeatureDigitalSignatures getDef tid
    FeatureScopeUser _ -> TeamFeatureStatusNoConfig <$> getDef
    FeatureScopeServer -> TeamFeatureStatusNoConfig <$> getDef
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure TeamFeatureDisabled

setDigitalSignaturesInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureDigitalSignatures r
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'TeamFeatureDigitalSignatures $ \_ _ -> pure ()

getLegalholdStatusInternal ::
  Members '[LegalHoldStore, TeamFeatureStore, TeamStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureLegalHold r
getLegalholdStatusInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    isLegalHoldEnabledForTeam tid <&> \case
      True -> TeamFeatureStatusNoConfig TeamFeatureEnabled
      False -> TeamFeatureStatusNoConfig TeamFeatureDisabled
  FeatureScopeUser _ -> pure $ TeamFeatureStatusNoConfig TeamFeatureDisabled
  FeatureScopeServer -> pure $ TeamFeatureStatusNoConfig TeamFeatureDisabled

setLegalholdStatusInternal ::
  forall p r.
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         ConversationStore,
         Error AuthenticationError,
         Error InternalError,
         ErrorS ('ActionDenied 'RemoveConversationMember),
         ErrorS 'CannotEnableLegalHoldServiceLargeTeam,
         ErrorS 'NotATeamMember,
         Error TeamFeatureError,
         ErrorS 'LegalHoldNotEnabled,
         ErrorS 'LegalHoldDisableUnimplemented,
         ErrorS 'LegalHoldServiceNotRegistered,
         ErrorS 'UserLegalHoldIllegalOperation,
         ErrorS 'LegalHoldCouldNotBlockConnections,
         ExternalAccess,
         FederatorAccess,
         FireAndForget,
         GundeckAccess,
         Input (Local ()),
         Input UTCTime,
         LegalHoldStore,
         ListItems LegacyPaging ConvId,
         MemberStore,
         TeamFeatureStore,
         TeamStore,
         TeamMemberStore p,
         P.TinyLog
       ]
      r
  ) =>
  FeatureSetter 'TeamFeatureLegalHold r
setLegalholdStatusInternal = Tagged $ \tid status@(tfwoStatus -> statusValue) -> do
  do
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

  -- we're good to update the status now.
  case statusValue of
    TeamFeatureDisabled -> removeSettings' @p tid
    TeamFeatureEnabled -> ensureNotTooLargeToActivateLegalHold tid
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureLegalHold tid status

getFileSharingInternal ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureFileSharing r
getFileSharingInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureFileSharing tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureFileSharing)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults)

determineFeatureStatus ::
  TeamFeatureStatusNoConfigAndLockStatus ->
  LockStatusValue ->
  Maybe TeamFeatureStatusNoConfig ->
  TeamFeatureStatusNoConfigAndLockStatus
determineFeatureStatus cfgDefault lockStatus mbFeatureStatus = case (lockStatus, mbFeatureStatus) of
  (Unlocked, Just featureStatus) ->
    TeamFeatureStatusNoConfigAndLockStatus
      (tfwoStatus featureStatus)
      lockStatus
  (Unlocked, Nothing) -> cfgDefault {tfwoapsLockStatus = lockStatus}
  (Locked, _) -> cfgDefault {tfwoapsLockStatus = lockStatus}

getFeatureStatusWithDefaultConfig ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    HasStatusCol a,
    FeatureHasNoConfig 'WithoutLockStatus a,
    Members '[Input Opts, TeamFeatureStore] r
  ) =>
  Lens' FeatureFlags (Defaults (TeamFeatureStatus 'WithoutLockStatus a)) ->
  Maybe TeamId ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus a)
getFeatureStatusWithDefaultConfig lens' =
  maybe
    (TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @a getDef)
  where
    getDef :: Sem r TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . lens'))
        <&> tfwoStatus . view unDefaults

setFileSharingInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamFeatureStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureFileSharing r
setFileSharingInternal = Tagged $ \tid status -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureFileSharing tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            TeamFeatureFileSharing
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (TeamFeatureStatusNoConfigAndLockStatus (tfwoStatus status) Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureFileSharing tid status <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults . to tfwoapsLockStatus)

getAppLockInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureAppLock r
getAppLockInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    mStatus <- TeamFeatures.getApplockFeatureStatus tid
    pure $ fromMaybe cfgDefault mStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault = do
      Defaults defaultStatus <- inputs (view (optSettings . setFeatureFlags . flagAppLockDefaults))
      pure defaultStatus

setAppLockInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, Error TeamFeatureError, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureAppLock r
setAppLockInternal = Tagged $ \tid status -> do
  when (applockInactivityTimeoutSecs (tfwcConfig status) < 30) $
    throw AppLockInactivityTimeoutTooLow
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update TeamFeatureAppLock (EdFeatureApplockChanged status)
  TeamFeatures.setApplockFeatureStatus tid status <* pushEvent

getClassifiedDomainsInternal ::
  Member (Input Opts) r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureClassifiedDomains r
getClassifiedDomainsInternal = Tagged . const $ do
  globalConfig <- inputs (view (optSettings . setFeatureFlags . flagClassifiedDomains))
  let config = globalConfig
  pure $ case tfwcStatus config of
    TeamFeatureDisabled -> defaultClassifiedDomains
    TeamFeatureEnabled -> config

getConferenceCallingInternal ::
  Members '[BrigAccess, Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureConferenceCalling r
getConferenceCallingInternal = Tagged $ \case
  FeatureScopeTeam tid -> getFeatureStatusWithDefaultConfig @'TeamFeatureConferenceCalling flagConferenceCalling (Just tid)
  FeatureScopeUser uid -> getFeatureConfigViaAccount @'TeamFeatureConferenceCalling uid
  FeatureScopeServer -> getFeatureStatusWithDefaultConfig @'TeamFeatureConferenceCalling flagConferenceCalling Nothing

setConferenceCallingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureConferenceCalling r
setConferenceCallingInternal =
  setFeatureStatusNoConfig @'TeamFeatureConferenceCalling $ \_status _tid -> pure ()

getSelfDeletingMessagesInternal ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureSelfDeletingMessages r
getSelfDeletingMessagesInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    let defLockStatus = tfwcapsLockStatus cfgDefault
    (mbFeatureStatus, fromMaybe defLockStatus -> lockStatus) <- TeamFeatures.getSelfDeletingMessagesStatus tid
    pure $ case (lockStatus, mbFeatureStatus) of
      (Unlocked, Just featureStatus) ->
        TeamFeatureStatusWithConfigAndLockStatus
          (tfwcStatus featureStatus)
          (tfwcConfig featureStatus)
          Unlocked
      (Unlocked, Nothing) -> cfgDefault {tfwcapsLockStatus = Unlocked}
      (Locked, _) -> cfgDefault {tfwcapsLockStatus = Locked}
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatusWithConfigAndLockStatus TeamFeatureSelfDeletingMessagesConfig)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults)

setSelfDeletingMessagesInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureSelfDeletingMessages r
setSelfDeletingMessagesInternal = Tagged $ \tid st -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureSelfDeletingMessages tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update TeamFeatureSelfDeletingMessages (EdFeatureSelfDeletingMessagesChanged st)
  TeamFeatures.setSelfDeletingMessagesStatus tid st <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults . to tfwcapsLockStatus)

getGuestLinkInternal ::
  forall r.
  (Member (Input Opts) r, Member TeamFeatureStore r) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureGuestLinks r
getGuestLinkInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureGuestLinks tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureGuestLinks)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults)

setGuestLinkInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureGuestLinks r
setGuestLinkInternal = Tagged $ \tid status -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureGuestLinks tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            TeamFeatureGuestLinks
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (TeamFeatureStatusNoConfigAndLockStatus (tfwoStatus status) Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureGuestLinks tid status <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults . to tfwoapsLockStatus)

getSndFactorPasswordChallengeInternal ::
  forall r.
  (Member (Input Opts) r, Member TeamFeatureStore r) =>
  FeatureGetter 'WithLockStatus 'TeamFeatureSndFactorPasswordChallenge r
getSndFactorPasswordChallengeInternal = Tagged $ \case
  FeatureScopeTeam tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureSndFactorPasswordChallenge tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  FeatureScopeUser _ -> getCfgDefault
  FeatureScopeServer -> getCfgDefault
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureSndFactorPasswordChallenge)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

getSndFactorPasswordChallengeNoAuth ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r,
    Member (ErrorS 'NotATeamMember) r,
    Member TeamStore r
  ) =>
  Maybe UserId ->
  Sem r TeamFeatureStatusNoConfig
getSndFactorPasswordChallengeNoAuth mbUserId = do
  scope <- getScope mbUserId
  TeamFeatureStatusNoConfig . tfwoapsStatus <$> unTagged getSndFactorPasswordChallengeInternal scope
  where
    getScope :: Maybe UserId -> Sem r FeatureScope
    getScope = \case
      Just uid -> do
        mbTeam <- getOneUserTeam uid
        case mbTeam of
          Nothing -> pure $ FeatureScopeUser uid
          Just tid -> do
            teamExists <- isJust <$> getTeam tid
            if teamExists
              then pure $ FeatureScopeTeam tid
              else pure $ FeatureScopeUser uid
      Nothing -> pure FeatureScopeServer

-- | If second factor auth is enabled, make sure that end-points that don't support it, but should, are blocked completely.  (This is a workaround until we have 2FA for those end-points as well.)
--
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
  teamFeature <- case mbCnvData >>= cnvmTeam of
    Nothing -> getSndFactorPasswordChallengeNoAuth (Just uid)
    Just tid -> do
      teamExists <- isJust <$> getTeam tid
      if teamExists
        then TeamFeatureStatusNoConfig . tfwoapsStatus <$> unTagged getSndFactorPasswordChallengeInternal (FeatureScopeTeam tid)
        else getSndFactorPasswordChallengeNoAuth (Just uid)
  case tfwoStatus teamFeature of
    TeamFeatureDisabled -> action
    TeamFeatureEnabled -> throwS @'AccessDenied

setSndFactorPasswordChallengeInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  FeatureSetter 'TeamFeatureSndFactorPasswordChallenge r
setSndFactorPasswordChallengeInternal = Tagged $ \tid status -> do
  getDftLockStatus >>= guardLockStatus @'TeamFeatureSndFactorPasswordChallenge tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            TeamFeatureSndFactorPasswordChallenge
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (TeamFeatureStatusNoConfigAndLockStatus (tfwoStatus status) Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'TeamFeatureSndFactorPasswordChallenge tid status <* pushEvent
  where
    getDftLockStatus :: Sem r LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults . to tfwoapsLockStatus)

getTeamSearchVisibilityInboundInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSearchVisibilityInbound r
getTeamSearchVisibilityInboundInternal =
  Tagged $ \case
    FeatureScopeTeam tid -> getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound (Just tid)
    FeatureScopeUser _ -> getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound Nothing
    FeatureScopeServer -> getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound Nothing

setTeamSearchVisibilityInboundInternal ::
  Members '[Error InternalError, GundeckAccess, TeamStore, TeamFeatureStore, BrigAccess, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureSearchVisibilityInbound r
setTeamSearchVisibilityInboundInternal = Tagged $ \tid status -> do
  updatedStatus <- unTagged (setFeatureStatusNoConfig @'TeamFeatureSearchVisibilityInbound $ \_ _ -> pure ()) tid status
  mPersistedStatus <- listToMaybe <$> TeamFeatures.getFeatureStatusNoConfigMulti (Proxy @'TeamFeatureSearchVisibilityInbound) [tid]
  case mPersistedStatus of
    Just (persistedTid, persistedStatus, persistedWriteTime) ->
      updateSearchVisibilityInbound $
        Multi.TeamStatusUpdate persistedTid persistedStatus persistedWriteTime
    Nothing -> throw (InternalErrorWithDescription "Failed to retrieve search-visibility-inbound status after persisting it")
  pure updatedStatus

getFeatureStatusMulti ::
  forall f r.
  ( KnownTeamFeatureName f,
    FeatureHasNoConfig 'WithoutLockStatus f,
    HasStatusCol f,
    Members
      '[ TeamStore,
         TeamFeatureStore,
         Input Opts
       ]
      r
  ) =>
  Lens' FeatureFlags (Defaults (TeamFeatureStatus 'WithoutLockStatus f)) ->
  (Multi.TeamFeatureNoConfigMultiRequest -> (Sem r) (Multi.TeamFeatureNoConfigMultiResponse 'TeamFeatureSearchVisibilityInbound))
getFeatureStatusMulti lens' (Multi.TeamFeatureNoConfigMultiRequest teams) = do
  triples <- TeamFeatures.getFeatureStatusNoConfigMulti (Proxy @f) teams
  let tsExplicit = map (\(tid, sv, t) -> Multi.TeamStatus tid sv (Just t)) triples
  let teamsDefault = Set.toList (Set.fromList teams `Set.difference` Set.fromList (Multi.team <$> tsExplicit))
  defaultStatus <- getDef
  let tsImplicit = [Multi.TeamStatus tid defaultStatus Nothing | tid <- teamsDefault]
  pure $ Multi.TeamFeatureNoConfigMultiResponse $ tsExplicit <> tsImplicit
  where
    getDef :: Sem r TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . lens'))
        <&> tfwoStatus . view unDefaults

getTeamSearchVisibilityInboundInternalMulti ::
  Members
    '[ TeamStore,
       TeamFeatureStore,
       Input Opts
     ]
    r =>
  Multi.TeamFeatureNoConfigMultiRequest ->
  (Sem r) (Multi.TeamFeatureNoConfigMultiResponse 'TeamFeatureSearchVisibilityInbound)
getTeamSearchVisibilityInboundInternalMulti =
  getFeatureStatusMulti @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound

-- TODO(fisx): move this function to a more suitable place / module.
guardLockStatus ::
  forall (a :: TeamFeatureName) r.
  ( MaybeHasLockStatusCol a,
    Member TeamFeatureStore r,
    Member (Error TeamFeatureError) r
  ) =>
  TeamId ->
  LockStatusValue -> -- FUTUREWORK(fisx): move this into its own type class and infer from `a`?
  Sem r ()
guardLockStatus tid defLockStatus = do
  (TeamFeatures.getLockStatus @a tid <&> fromMaybe defLockStatus) >>= \case
    Unlocked -> pure ()
    Locked -> throw FeatureLocked

pushFeatureConfigEvent ::
  Members '[GundeckAccess, TeamStore, P.TinyLog] r =>
  TeamId ->
  Event.Event ->
  Sem r ()
pushFeatureConfigEvent tid event = do
  memList <- getTeamMembersForFanout tid
  when ((memList ^. teamMemberListType) == ListTruncated) $ do
    P.warn $
      Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
        . Log.field "feature" (Log.val (toByteString' . Event._eventFeatureName $ event))
        . Log.field "team" (Log.val (cs . show $ tid))
        . Log.msg @Text "Fanout limit exceeded. Some events will not be sent."
  let recipients = membersToRecipients Nothing (memList ^. teamMembers)
  for_
    (newPush (memList ^. teamMemberListType) Nothing (FeatureConfigEvent event) recipients)
    push1

-- | (Currently, we only have 'TeamFeatureConferenceCalling' here, but we may have to
-- extend this in the future.)
getFeatureConfigViaAccount ::
  ( flag ~ 'TeamFeatureConferenceCalling,
    Member BrigAccess r
  ) =>
  UserId ->
  Sem r (TeamFeatureStatus 'WithoutLockStatus flag)
getFeatureConfigViaAccount = getAccountFeatureConfigClient
