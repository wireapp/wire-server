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
    getFeatureStatusNoConfig,
    setFeatureStatus,
    getFeatureConfig,
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
    getGuestLinkInternal,
    setGuestLinkInternal,
    setLockStatus,
    DoAuth (..),
    GetFeatureInternalParam,
  )
where

import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Conversion hiding (fromList)
import Data.Either.Extra (eitherToMaybe)
import Data.Id
import Data.Qualified
import Data.String.Conversions (cs)
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
import Wire.API.ErrorDescription
import Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.FeatureConfig as Event
import Wire.API.Federation.Error
import Wire.API.Team.Feature (AllFeatureConfigs (..), FeatureHasNoConfig, KnownTeamFeatureName, TeamFeatureName)
import qualified Wire.API.Team.Feature as Public

data DoAuth = DoAuth UserId | DontDoAuth

-- | For team-settings, to administrate team feature configuration.  Here we have an admin uid
-- and a team id, but no uid of the member for which the feature config holds.
getFeatureStatus ::
  forall (ps :: Public.IncludeLockStatus) (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore
       ]
      r
  ) =>
  (GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
  DoAuth ->
  TeamId ->
  Sem r (Public.TeamFeatureStatus ps a)
getFeatureStatus getter doauth tid = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getter (Right tid)

-- | For team-settings, like 'getFeatureStatus'.
setFeatureStatus ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    MaybeHasLockStatusCol a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore,
         TeamFeatureStore
       ]
      r
  ) =>
  (TeamId -> Public.TeamFeatureStatus 'Public.WithoutLockStatus a -> Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)) ->
  DoAuth ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus a ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
setFeatureStatus setter doauth tid status = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck (ChangeTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  setter tid status

-- | Setting lock status can only be done through the internal API and therefore doesn't require auth.
setLockStatus ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    HasLockStatusCol a,
    Members
      [ Error ActionError,
        Error TeamError,
        Error NotATeamMember,
        TeamStore,
        TeamFeatureStore
      ]
      r
  ) =>
  TeamId ->
  Public.LockStatusValue ->
  Sem r Public.LockStatus
setLockStatus tid lockStatusUpdate = do
  assertTeamExists tid
  TeamFeatures.setLockStatus @a tid (Public.LockStatus lockStatusUpdate)

-- | For individual users to get feature config for their account (personal or team).
getFeatureConfig ::
  forall (ps :: Public.IncludeLockStatus) (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore
       ]
      r
  ) =>
  (GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
  UserId ->
  Sem r (Public.TeamFeatureStatus ps a)
getFeatureConfig getter zusr = do
  mbTeam <- getOneUserTeam zusr
  case mbTeam of
    Nothing -> getter (Left (Just zusr))
    Just tid -> do
      zusrMembership <- getTeamMember tid zusr
      void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
      assertTeamExists tid
      getter (Right tid)

getAllFeatureConfigs ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error NotATeamMember,
       Error TeamError,
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
        forall (ps :: Public.IncludeLockStatus) (a :: Public.TeamFeatureName) r.
        ( Public.KnownTeamFeatureName a,
          Aeson.ToJSON (Public.TeamFeatureStatus ps a),
          Members '[Error ActionError, Error TeamError, Error NotATeamMember, TeamStore] r
        ) =>
        (GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
        Sem r (Aeson.Key, Aeson.Value)
      getStatus getter = do
        when (isJust mbTeam) $ do
          void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
        status <- getter (maybe (Left (Just zusr)) Right mbTeam)
        let feature = Public.knownTeamFeatureName @a
        pure $ AesonKey.fromText (cs (toByteString' feature)) Aeson..= status

  AllFeatureConfigs . KeyMap.fromList
    <$> sequence
      [ getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureGuestLinks getGuestLinkInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureSndFactorPasswordChallenge getSndFactorPasswordChallengeInternal
      ]

getAllFeaturesH ::
  Members
    '[ BrigAccess,
       Error ActionError,
       Error TeamError,
       Error NotATeamMember,
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
       Error ActionError,
       Error TeamError,
       Error NotATeamMember,
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
      [ getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.WithoutLockStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureGuestLinks getGuestLinkInternal,
        getStatus @'Public.WithLockStatus @'Public.TeamFeatureSndFactorPasswordChallenge getSndFactorPasswordChallengeInternal
      ]
  where
    getStatus ::
      forall (ps :: Public.IncludeLockStatus) (a :: Public.TeamFeatureName).
      ( Public.KnownTeamFeatureName a,
        Aeson.ToJSON (Public.TeamFeatureStatus ps a)
      ) =>
      (GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
      Sem r (Aeson.Key, Aeson.Value)
    getStatus getter = do
      status <- getFeatureStatus @ps @a getter (DoAuth uid) tid
      let feature = Public.knownTeamFeatureName @a
      pure $ AesonKey.fromText (cs (toByteString' feature)) Aeson..= status

getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    HasStatusCol a,
    Member TeamFeatureStore r
  ) =>
  Sem r Public.TeamFeatureStatusValue ->
  TeamId ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- Public.TeamFeatureStatusNoConfig <$> getDefault
  fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @a tid

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig 'Public.WithoutLockStatus a,
    HasStatusCol a,
    Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r
  ) =>
  (Public.TeamFeatureStatusValue -> TeamId -> Sem r ()) ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus a ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
setFeatureStatusNoConfig applyState tid status = do
  applyState (Public.tfwoStatus status) tid
  newStatus <- TeamFeatures.setFeatureStatusNoConfig @a tid status
  pushFeatureConfigEvent tid $
    Event.Event Event.Update (Public.knownTeamFeatureName @a) (EdFeatureWithoutConfigChanged newStatus)
  pure newStatus

-- | FUTUREWORK(fisx): (thanks pcapriotti) this should probably be a type family dependent on
-- the feature flag, so that we get more type safety.
type GetFeatureInternalParam = Either (Maybe UserId) TeamId

getSSOStatusInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSSO)
getSSOStatusInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureSSO getDef)
  where
    getDef :: Member (Input Opts) r => Sem r Public.TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . flagSSO)) <&> \case
        FeatureSSOEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureSSODisabledByDefault -> Public.TeamFeatureDisabled

setSSOStatusInternal ::
  Members '[Error TeamFeatureError, GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSSO ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSSO)
setSSOStatusInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSSO $ \case
  Public.TeamFeatureDisabled -> const (throw DisableSsoNotImplemented)
  Public.TeamFeatureEnabled -> const (pure ())

getTeamSearchVisibilityAvailableInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility getDef)
  where
    getDef = do
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal ::
  Members '[GundeckAccess, SearchVisibilityStore, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSearchVisibility ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSearchVisibility)
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility $ \case
  Public.TeamFeatureDisabled -> SearchVisibilityData.resetSearchVisibility
  Public.TeamFeatureEnabled -> const (pure ())

getValidateSAMLEmailsInternal ::
  forall r.
  ( Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureValidateSAMLEmails)
getValidateSAMLEmailsInternal =
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureValidateSAMLEmails
    flagsTeamFeatureValidateSAMLEmailsStatus
    . eitherToMaybe

setValidateSAMLEmailsInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureValidateSAMLEmails ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureValidateSAMLEmails)
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureDigitalSignatures)
getDigitalSignaturesInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setDigitalSignaturesInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureDigitalSignatures ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureDigitalSignatures)
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures $ \_ _ -> pure ()

getLegalholdStatusInternal ::
  Members '[LegalHoldStore, TeamFeatureStore, TeamStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureLegalHold)
getLegalholdStatusInternal (Left _) =
  pure $ Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled
getLegalholdStatusInternal (Right tid) = do
  isLegalHoldEnabledForTeam tid <&> \case
    True -> Public.TeamFeatureStatusNoConfig Public.TeamFeatureEnabled
    False -> Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled

setLegalholdStatusInternal ::
  forall p r.
  ( Paging p,
    Bounded (PagingBounds p TeamMember),
    Members
      '[ BotAccess,
         BrigAccess,
         CodeStore,
         ConversationStore,
         Error ActionError,
         Error AuthenticationError,
         Error ConversationError,
         Error FederationError,
         Error InvalidInput,
         Error LegalHoldError,
         Error TeamError,
         Error NotATeamMember,
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
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureLegalHold ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureLegalHold)
setLegalholdStatusInternal tid status@(Public.tfwoStatus -> statusValue) = do
  do
    -- this extra do is to encapsulate the assertions running before the actual operation.
    -- enabeling LH for teams is only allowed in normal operation; disabled-permanently and
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
    Public.TeamFeatureDisabled -> removeSettings' @p tid
    Public.TeamFeatureEnabled -> do
      ensureNotTooLargeToActivateLegalHold tid
  TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid status

getFileSharingInternal ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureFileSharing)
getFileSharingInternal = \case
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (Public.tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'Public.TeamFeatureFileSharing tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  where
    getCfgDefault :: Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureFileSharing)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults)

determineFeatureStatus ::
  Public.TeamFeatureStatusNoConfigAndLockStatus ->
  Public.LockStatusValue ->
  Maybe Public.TeamFeatureStatusNoConfig ->
  Public.TeamFeatureStatusNoConfigAndLockStatus
determineFeatureStatus cfgDefault lockStatus mbFeatureStatus = case (lockStatus, mbFeatureStatus) of
  (Public.Unlocked, Just featureStatus) ->
    Public.TeamFeatureStatusNoConfigAndLockStatus
      (Public.tfwoStatus featureStatus)
      lockStatus
  (Public.Unlocked, Nothing) -> cfgDefault {Public.tfwoapsLockStatus = lockStatus}
  (Public.Locked, _) -> cfgDefault {Public.tfwoapsLockStatus = lockStatus}

getFeatureStatusWithDefaultConfig ::
  forall (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    HasStatusCol a,
    FeatureHasNoConfig 'Public.WithoutLockStatus a,
    Members '[Input Opts, TeamFeatureStore] r
  ) =>
  Lens' FeatureFlags (Defaults (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)) ->
  Maybe TeamId ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus a)
getFeatureStatusWithDefaultConfig lens' =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @a getDef)
  where
    getDef :: Sem r Public.TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . lens'))
        <&> Public.tfwoStatus . view unDefaults

setFileSharingInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamFeatureStore r,
    Member TeamStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureFileSharing ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureFileSharing)
setFileSharingInternal tid status = do
  getDftLockStatus >>= guardLockStatus @'Public.TeamFeatureFileSharing tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            Public.TeamFeatureFileSharing
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (Public.TeamFeatureStatusNoConfigAndLockStatus (Public.tfwoStatus status) Public.Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureFileSharing tid status <* pushEvent
  where
    getDftLockStatus :: Sem r Public.LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagFileSharing . unDefaults . to Public.tfwoapsLockStatus)

getAppLockInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureAppLock)
getAppLockInternal mbtid = do
  Defaults defaultStatus <- inputs (view (optSettings . setFeatureFlags . flagAppLockDefaults))
  status <-
    join <$> (TeamFeatures.getApplockFeatureStatus `mapM` either (const Nothing) Just mbtid)
  pure $ fromMaybe defaultStatus status

setAppLockInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, Error TeamFeatureError, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureAppLock ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureAppLock)
setAppLockInternal tid status = do
  when (Public.applockInactivityTimeoutSecs (Public.tfwcConfig status) < 30) $
    throw AppLockinactivityTimeoutTooLow
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update Public.TeamFeatureAppLock (EdFeatureApplockChanged status)
  TeamFeatures.setApplockFeatureStatus tid status <* pushEvent

getClassifiedDomainsInternal ::
  Member (Input Opts) r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureClassifiedDomains)
getClassifiedDomainsInternal _mbtid = do
  globalConfig <- inputs (view (optSettings . setFeatureFlags . flagClassifiedDomains))
  let config = globalConfig
  pure $ case Public.tfwcStatus config of
    Public.TeamFeatureDisabled -> Public.defaultClassifiedDomains
    Public.TeamFeatureEnabled -> config

getConferenceCallingInternal ::
  Members '[BrigAccess, Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureConferenceCalling)
getConferenceCallingInternal (Left (Just uid)) = do
  getFeatureConfigViaAccount @'Public.TeamFeatureConferenceCalling uid
getConferenceCallingInternal (Left Nothing) = do
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureConferenceCalling flagConferenceCalling Nothing
getConferenceCallingInternal (Right tid) = do
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureConferenceCalling flagConferenceCalling (Just tid)

setConferenceCallingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureConferenceCalling ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureConferenceCalling)
setConferenceCallingInternal =
  setFeatureStatusNoConfig @'Public.TeamFeatureConferenceCalling $ \_status _tid -> pure ()

getSelfDeletingMessagesInternal ::
  forall r.
  ( Member (Input Opts) r,
    Member TeamFeatureStore r
  ) =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureSelfDeletingMessages)
getSelfDeletingMessagesInternal = \case
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    let defLockStatus = Public.tfwcapsLockStatus cfgDefault
    (mbFeatureStatus, fromMaybe defLockStatus -> lockStatus) <- TeamFeatures.getSelfDeletingMessagesStatus tid
    pure $ case (lockStatus, mbFeatureStatus) of
      (Public.Unlocked, Just featureStatus) ->
        Public.TeamFeatureStatusWithConfigAndLockStatus
          (Public.tfwcStatus featureStatus)
          (Public.tfwcConfig featureStatus)
          Public.Unlocked
      (Public.Unlocked, Nothing) -> cfgDefault {Public.tfwcapsLockStatus = Public.Unlocked}
      (Public.Locked, _) -> cfgDefault {Public.tfwcapsLockStatus = Public.Locked}
  where
    getCfgDefault :: Sem r (Public.TeamFeatureStatusWithConfigAndLockStatus Public.TeamFeatureSelfDeletingMessagesConfig)
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
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSelfDeletingMessages ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSelfDeletingMessages)
setSelfDeletingMessagesInternal tid st = do
  getDftLockStatus >>= guardLockStatus @'Public.TeamFeatureSelfDeletingMessages tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event Event.Update Public.TeamFeatureSelfDeletingMessages (EdFeatureSelfDeletingMessagesChanged st)
  TeamFeatures.setSelfDeletingMessagesStatus tid st <* pushEvent
  where
    getDftLockStatus :: Sem r Public.LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagSelfDeletingMessages . unDefaults . to Public.tfwcapsLockStatus)

getGuestLinkInternal ::
  forall r.
  (Member (Input Opts) r, Member TeamFeatureStore r) =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureGuestLinks)
getGuestLinkInternal = \case
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (Public.tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'Public.TeamFeatureGuestLinks tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  where
    getCfgDefault :: Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureGuestLinks)
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
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureGuestLinks ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureGuestLinks)
setGuestLinkInternal tid status = do
  getDftLockStatus >>= guardLockStatus @'Public.TeamFeatureGuestLinks tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            Public.TeamFeatureGuestLinks
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (Public.TeamFeatureStatusNoConfigAndLockStatus (Public.tfwoStatus status) Public.Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureGuestLinks tid status <* pushEvent
  where
    getDftLockStatus :: Sem r Public.LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagConversationGuestLinks . unDefaults . to Public.tfwoapsLockStatus)

getSndFactorPasswordChallengeInternal ::
  forall r.
  (Member (Input Opts) r, Member TeamFeatureStore r) =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureSndFactorPasswordChallenge)
getSndFactorPasswordChallengeInternal = \case
  Left _ -> getCfgDefault
  Right tid -> do
    cfgDefault <- getCfgDefault
    (mbFeatureStatus, fromMaybe (Public.tfwoapsLockStatus cfgDefault) -> lockStatus) <- TeamFeatures.getFeatureStatusNoConfigAndLockStatus @'Public.TeamFeatureSndFactorPasswordChallenge tid
    pure $ determineFeatureStatus cfgDefault lockStatus mbFeatureStatus
  where
    getCfgDefault :: Sem r (Public.TeamFeatureStatus 'Public.WithLockStatus 'Public.TeamFeatureSndFactorPasswordChallenge)
    getCfgDefault = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults)

setSndFactorPasswordChallengeInternal ::
  forall r.
  ( Member GundeckAccess r,
    Member TeamStore r,
    Member TeamFeatureStore r,
    Member P.TinyLog r,
    Member (Error TeamFeatureError) r,
    Member (Input Opts) r
  ) =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSndFactorPasswordChallenge ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus 'Public.TeamFeatureSndFactorPasswordChallenge)
setSndFactorPasswordChallengeInternal tid status = do
  getDftLockStatus >>= guardLockStatus @'Public.TeamFeatureSndFactorPasswordChallenge tid
  let pushEvent =
        pushFeatureConfigEvent tid $
          Event.Event
            Event.Update
            Public.TeamFeatureSndFactorPasswordChallenge
            ( EdFeatureWithoutConfigAndLockStatusChanged
                (Public.TeamFeatureStatusNoConfigAndLockStatus (Public.tfwoStatus status) Public.Unlocked)
            )
  TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureSndFactorPasswordChallenge tid status <* pushEvent
  where
    getDftLockStatus :: Sem r Public.LockStatusValue
    getDftLockStatus = input <&> view (optSettings . setFeatureFlags . flagTeamFeatureSndFactorPasswordChallengeStatus . unDefaults . to Public.tfwoapsLockStatus)

-- TODO(fisx): move this function to a more suitable place / module.
guardLockStatus ::
  forall (a :: Public.TeamFeatureName) r.
  ( MaybeHasLockStatusCol a,
    Member TeamFeatureStore r,
    Member (Error TeamFeatureError) r
  ) =>
  TeamId ->
  Public.LockStatusValue -> -- FUTUREWORK(fisx): move this into its own type class and infer from `a`?
  Sem r ()
guardLockStatus tid defLockStatus = do
  (TeamFeatures.getLockStatus @a tid <&> fromMaybe defLockStatus) >>= \case
    Public.Unlocked -> pure ()
    Public.Locked -> throw FeatureLocked

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

-- | (Currently, we only have 'Public.TeamFeatureConferenceCalling' here, but we may have to
-- extend this in the future.)
getFeatureConfigViaAccount ::
  ( flag ~ 'Public.TeamFeatureConferenceCalling,
    Member BrigAccess r
  ) =>
  UserId ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutLockStatus flag)
getFeatureConfigViaAccount = getAccountFeatureConfigClient
