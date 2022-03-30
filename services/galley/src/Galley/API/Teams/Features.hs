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
    getFeatureConfigNoAuth,
    getAllFeatureConfigs,
    getAllFeaturesH,
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
    GetFeatureInternalParam,
    guardSecondFactorDisabled,
  )
where

import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Conversion hiding (fromList)
import Data.Either.Extra (eitherToMaybe)
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
import Galley.Effects.GundeckAccess
import Galley.Effects.Paging
import qualified Galley.Effects.SearchVisibilityStore as SearchVisibilityData
import qualified Galley.Effects.TeamFeatureStore as TeamFeatures
import Galley.Effects.TeamStore
import Galley.Intra.Push (PushEvent (FeatureConfigEvent), newPush)
import Galley.Options
import Galley.Types.Teams hiding (newTeam)
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus)
import Network.Wai.Utilities hiding (Error)
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

type FeatureGetter l f r = Tagged '(l, f) (GetFeatureInternalParam -> Sem r (TeamFeatureStatus l f))

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
      void $ permissionCheck (ViewTeamFeature (knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getter (Right tid)

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
      void $ permissionCheck (ChangeTeamFeature (knownTeamFeatureName @a)) zusrMembership
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
    Nothing -> getter (Left (Just zusr))
    Just tid -> do
      zusrMembership <- getTeamMember tid zusr
      void $ permissionCheck (ViewTeamFeature (knownTeamFeatureName @a)) zusrMembership
      assertTeamExists tid
      getter (Right tid)

getFeatureConfigNoAuth ::
  forall (ps :: IncludeLockStatus) (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    Members
      '[ ErrorS 'NotATeamMember,
         TeamStore
       ]
      r
  ) =>
  (GetFeatureInternalParam -> Sem r (TeamFeatureStatus ps a)) ->
  UserId ->
  Sem r (TeamFeatureStatus ps a)
getFeatureConfigNoAuth getter zusr = do
  mbTeam <- getOneUserTeam zusr
  case mbTeam of
    Nothing -> getter (Left (Just zusr))
    Just tid -> do
      teamExists <- isJust <$> getTeam tid
      if teamExists
        then getter (Right tid)
        else getter (Left (Just zusr))

getAllFeatureConfigs ::
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
getAllFeatureConfigs zusr = do
  mbTeam <- getOneUserTeam zusr
  zusrMembership <- maybe (pure Nothing) (flip getTeamMember zusr) mbTeam
  let getStatus ::
        forall (ps :: IncludeLockStatus) (a :: TeamFeatureName) r.
        ( KnownTeamFeatureName a,
          Aeson.ToJSON (TeamFeatureStatus ps a),
          Members '[ErrorS 'NotATeamMember, ErrorS OperationDenied, TeamStore] r
        ) =>
        FeatureGetter ps a r ->
        Sem r (Aeson.Key, Aeson.Value)
      getStatus (Tagged getter) = do
        when (isJust mbTeam) $ do
          void $ permissionCheck (ViewTeamFeature (knownTeamFeatureName @a)) zusrMembership
        status <- getter (maybe (Left (Just zusr)) Right mbTeam)
        let feature = knownTeamFeatureName @a
        pure $ AesonKey.fromText (cs (toByteString' feature)) Aeson..= status

  AllFeatureConfigs . KeyMap.fromList
    <$> sequence
      [ getStatus @'WithoutLockStatus @'TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureSSO getSSOStatusInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureAppLock getAppLockInternal,
        getStatus @'WithLockStatus @'TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'WithLockStatus @'TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal,
        getStatus @'WithLockStatus @'TeamFeatureGuestLinks getGuestLinkInternal,
        getStatus @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge getSndFactorPasswordChallengeInternal
      ]

getAllFeaturesH ::
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
  UserId ::: TeamId ::: JSON ->
  Sem r Response
getAllFeaturesH (uid ::: tid ::: _) =
  json <$> getAllFeatures uid tid

getAllFeatures ::
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
  UserId ->
  TeamId ->
  Sem r Aeson.Value
getAllFeatures uid tid = do
  Aeson.object
    <$> sequence
      [ getStatus @'WithoutLockStatus @'TeamFeatureSSO getSSOStatusInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureAppLock getAppLockInternal,
        getStatus @'WithLockStatus @'TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'WithoutLockStatus @'TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'WithLockStatus @'TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal,
        getStatus @'WithLockStatus @'TeamFeatureGuestLinks getGuestLinkInternal,
        getStatus @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge getSndFactorPasswordChallengeInternal
      ]
  where
    getStatus ::
      forall (ps :: IncludeLockStatus) (a :: TeamFeatureName).
      ( KnownTeamFeatureName a,
        Aeson.ToJSON (TeamFeatureStatus ps a)
      ) =>
      FeatureGetter ps a r ->
      Sem r (Aeson.Key, Aeson.Value)
    getStatus getter = do
      status <- getFeatureStatus @ps @a getter (DoAuth uid) tid
      let feature = knownTeamFeatureName @a
      pure $ AesonKey.fromText (cs (toByteString' feature)) Aeson..= status

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

-- | FUTUREWORK(fisx): (thanks pcapriotti) this should probably be a type family dependent on
-- the feature flag, so that we get more type safety.
type GetFeatureInternalParam = Either (Maybe UserId) TeamId

getSSOStatusInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureSSO r
getSSOStatusInternal =
  Tagged $
    either
      (const $ TeamFeatureStatusNoConfig <$> getDef)
      (getFeatureStatusNoConfig @'TeamFeatureSSO getDef)
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
  Tagged $
    either
      (const $ TeamFeatureStatusNoConfig <$> getDef)
      (getFeatureStatusNoConfig @'TeamFeatureSearchVisibility getDef)
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
  Tagged $
    getFeatureStatusWithDefaultConfig @'TeamFeatureValidateSAMLEmails
      flagsTeamFeatureValidateSAMLEmailsStatus
      . eitherToMaybe

setValidateSAMLEmailsInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  FeatureSetter 'TeamFeatureValidateSAMLEmails r
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal ::
  Member TeamFeatureStore r =>
  FeatureGetter 'WithoutLockStatus 'TeamFeatureDigitalSignatures r
getDigitalSignaturesInternal =
  Tagged $
    either
      (const $ TeamFeatureStatusNoConfig <$> getDef)
      (getFeatureStatusNoConfig @'TeamFeatureDigitalSignatures getDef)
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
  (Left _) -> pure $ TeamFeatureStatusNoConfig TeamFeatureDisabled
  (Right tid) -> do
    isLegalHoldEnabledForTeam tid <&> \case
      True -> TeamFeatureStatusNoConfig TeamFeatureEnabled
      False -> TeamFeatureStatusNoConfig TeamFeatureDisabled

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
         Error LegalHoldError,
         ErrorS ('ActionDenied 'RemoveConversationMember),
         ErrorS 'CannotEnableLegalHoldServiceLargeTeam,
         ErrorS 'NotATeamMember,
         Error TeamFeatureError,
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
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureFileSharing tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
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
getAppLockInternal = Tagged $ \mbtid -> do
  Defaults defaultStatus <- inputs (view (optSettings . setFeatureFlags . flagAppLockDefaults))
  status <-
    join <$> (TeamFeatures.getApplockFeatureStatus `mapM` either (const Nothing) Just mbtid)
  pure $ fromMaybe defaultStatus status

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
  (Left (Just uid)) -> getFeatureConfigViaAccount @'TeamFeatureConferenceCalling uid
  (Left Nothing) ->
    getFeatureStatusWithDefaultConfig @'TeamFeatureConferenceCalling flagConferenceCalling Nothing
  (Right tid) ->
    getFeatureStatusWithDefaultConfig @'TeamFeatureConferenceCalling flagConferenceCalling (Just tid)

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
  Left _ -> getCfgDefault
  Right tid -> do
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
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureGuestLinks tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
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
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'TeamFeatureSndFactorPasswordChallenge tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  where
    getCfgDefault :: Sem r (TeamFeatureStatus 'WithLockStatus 'TeamFeatureSndFactorPasswordChallenge)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

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
    Member TeamStore r
  ) =>
  UserId ->
  Sem r a ->
  Sem r a
guardSecondFactorDisabled uid action = do
  featureConfig <- getFeatureConfig @'WithLockStatus @'TeamFeatureSndFactorPasswordChallenge getSndFactorPasswordChallengeInternal uid
  case tfwoapsStatus featureConfig of
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
  Tagged $
    either
      (const $ getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound Nothing)
      (getFeatureStatusWithDefaultConfig @'TeamFeatureSearchVisibilityInbound flagTeamFeatureSearchVisibilityInbound . Just)

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
