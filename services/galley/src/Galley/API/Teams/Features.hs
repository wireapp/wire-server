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

module Galley.API.Teams.Features
  ( getFeatureStatus,
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
    setPaymentStatus,
    DoAuth (..),
    GetFeatureInternalParam,
  )
where

import Control.Lens
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion hiding (fromList)
import qualified Data.HashMap.Strict as HashMap
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
import Wire.API.Federation.Client
import Wire.API.Team.Feature (AllFeatureConfigs (..), FeatureHasNoConfig, KnownTeamFeatureName, TeamFeatureName)
import qualified Wire.API.Team.Feature as Public

data DoAuth = DoAuth UserId | DontDoAuth

-- | For team-settings, to administrate team feature configuration.  Here we have an admin uid
-- and a team id, but no uid of the member for which the feature config holds.
getFeatureStatus ::
  forall (ps :: Public.IncludePaymentStatus) (a :: Public.TeamFeatureName) r.
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
    MaybeHasPaymentStatusCol a,
    Members
      '[ Error ActionError,
         Error TeamError,
         Error NotATeamMember,
         TeamStore,
         TeamFeatureStore
       ]
      r
  ) =>
  (TeamId -> Public.TeamFeatureStatus 'Public.WithoutPaymentStatus a -> Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus a)) ->
  DoAuth ->
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus a ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus a)
setFeatureStatus setter doauth tid status = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- getTeamMember tid uid
      void $ permissionCheck (ChangeTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  setter tid status

-- | Setting payment status can only be done through the internal API and therefore doesn't require auth.
setPaymentStatus ::
  forall (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    HasPaymentStatusCol a,
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
  Public.PaymentStatusValue ->
  Sem r Public.PaymentStatus
setPaymentStatus tid paymentStatusUpdate = do
  assertTeamExists tid
  TeamFeatures.setPaymentStatus @a tid (Public.PaymentStatus paymentStatusUpdate)

-- | For individual users to get feature config for their account (personal or team).
getFeatureConfig ::
  forall (ps :: Public.IncludePaymentStatus) (a :: Public.TeamFeatureName) r.
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
        forall (ps :: Public.IncludePaymentStatus) (a :: Public.TeamFeatureName) r.
        ( Public.KnownTeamFeatureName a,
          Aeson.ToJSON (Public.TeamFeatureStatus ps a),
          Members '[Error ActionError, Error TeamError, Error NotATeamMember, TeamStore] r
        ) =>
        (GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
        Sem r (Text, Aeson.Value)
      getStatus getter = do
        when (isJust mbTeam) $ do
          void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
        status <- getter (maybe (Left (Just zusr)) Right mbTeam)
        let feature = Public.knownTeamFeatureName @a
        pure $ cs (toByteString' feature) Aeson..= status

  AllFeatureConfigs . HashMap.fromList
    <$> sequence
      [ getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'Public.WithPaymentStatus @'Public.TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal
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
      [ getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.WithoutPaymentStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal,
        getStatus @'Public.WithPaymentStatus @'Public.TeamFeatureSelfDeletingMessages getSelfDeletingMessagesInternal
      ]
  where
    getStatus ::
      forall (ps :: Public.IncludePaymentStatus) (a :: Public.TeamFeatureName).
      ( Public.KnownTeamFeatureName a,
        Aeson.ToJSON (Public.TeamFeatureStatus ps a)
      ) =>
      (GetFeatureInternalParam -> Sem r (Public.TeamFeatureStatus ps a)) ->
      Sem r (Text, Aeson.Value)
    getStatus getter = do
      status <- getFeatureStatus @ps @a getter (DoAuth uid) tid
      let feature = Public.knownTeamFeatureName @a
      pure $ cs (toByteString' feature) Aeson..= status

getFeatureStatusNoConfig ::
  forall (ps :: Public.IncludePaymentStatus) (a :: Public.TeamFeatureName) r.
  ( Public.FeatureHasNoConfig ps a,
    HasStatusCol a,
    Member TeamFeatureStore r
  ) =>
  Sem r Public.TeamFeatureStatusValue ->
  TeamId ->
  Sem r (Public.TeamFeatureStatus ps a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- Public.TeamFeatureStatusNoConfig <$> getDefault
  fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @ps @a tid

setFeatureStatusNoConfig ::
  forall (ps :: Public.IncludePaymentStatus) (a :: Public.TeamFeatureName) r.
  ( Public.KnownTeamFeatureName a,
    Public.FeatureHasNoConfig ps a,
    HasStatusCol a,
    Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r
  ) =>
  (Public.TeamFeatureStatusValue -> TeamId -> Sem r ()) ->
  TeamId ->
  Public.TeamFeatureStatus ps a ->
  Sem r (Public.TeamFeatureStatus ps a)
setFeatureStatusNoConfig applyState tid status = do
  applyState (Public.tfwoStatus status) tid
  newStatus <- TeamFeatures.setFeatureStatusNoConfig @ps @a tid status
  pushFeatureConfigEvent tid $
    Event.Event Event.Update (Public.knownTeamFeatureName @a) (EdFeatureWithoutConfigChanged newStatus)
  pure newStatus

-- | FUTUREWORK(fisx): (thanks pcapriotti) this should probably be a type family dependent on
-- the feature flag, so that we get more type safety.
type GetFeatureInternalParam = Either (Maybe UserId) TeamId

getSSOStatusInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSSO)
getSSOStatusInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureSSO getDef)
  where
    getDef :: Member (Input Opts) r => Sem r Public.TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . flagSSO)) <&> \case
        FeatureSSOEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureSSODisabledByDefault -> Public.TeamFeatureDisabled

setSSOStatusInternal ::
  Members '[Error TeamFeatureError, GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSSO ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSSO)
setSSOStatusInternal = setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureSSO $ \case
  Public.TeamFeatureDisabled -> const (throw DisableSsoNotImplemented)
  Public.TeamFeatureEnabled -> const (pure ())

getTeamSearchVisibilityAvailableInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureSearchVisibility getDef)
  where
    getDef = do
      inputs (view (optSettings . setFeatureFlags . flagTeamSearchVisibility)) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal ::
  Members '[GundeckAccess, SearchVisibilityStore, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSearchVisibility ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSearchVisibility)
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureSearchVisibility $ \case
  Public.TeamFeatureDisabled -> SearchVisibilityData.resetSearchVisibility
  Public.TeamFeatureEnabled -> const (pure ())

getValidateSAMLEmailsInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureValidateSAMLEmails)
getValidateSAMLEmailsInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureValidateSAMLEmails getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setValidateSAMLEmailsInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureValidateSAMLEmails ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureValidateSAMLEmails)
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureDigitalSignatures)
getDigitalSignaturesInternal =
  either
    (const $ Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureDigitalSignatures getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setDigitalSignaturesInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureDigitalSignatures ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureDigitalSignatures)
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureDigitalSignatures $ \_ _ -> pure ()

getLegalholdStatusInternal ::
  Members '[LegalHoldStore, TeamFeatureStore, TeamStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureLegalHold)
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
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureLegalHold ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureLegalHold)
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
  TeamFeatures.setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureLegalHold tid status

getFileSharingInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureFileSharing)
getFileSharingInternal =
  getFeatureStatusWithDefaultConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureFileSharing flagFileSharing . either (const Nothing) Just

getFeatureStatusWithDefaultConfig ::
  forall (ps :: Public.IncludePaymentStatus) (a :: TeamFeatureName) r.
  ( KnownTeamFeatureName a,
    HasStatusCol a,
    FeatureHasNoConfig ps a,
    Members '[Input Opts, TeamFeatureStore] r
  ) =>
  Lens' FeatureFlags (Defaults (Public.TeamFeatureStatus ps a)) ->
  Maybe TeamId ->
  Sem r (Public.TeamFeatureStatus ps a)
getFeatureStatusWithDefaultConfig lens' =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @ps @a getDef)
  where
    getDef :: Sem r Public.TeamFeatureStatusValue
    getDef =
      inputs (view (optSettings . setFeatureFlags . lens'))
        <&> Public.tfwoStatus . view unDefaults

setFileSharingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureFileSharing ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureFileSharing)
setFileSharingInternal = setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureFileSharing $ \_status _tid -> pure ()

getAppLockInternal ::
  Members '[Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureAppLock)
getAppLockInternal mbtid = do
  Defaults defaultStatus <- inputs (view (optSettings . setFeatureFlags . flagAppLockDefaults))
  status <-
    join <$> (TeamFeatures.getApplockFeatureStatus `mapM` either (const Nothing) Just mbtid)
  pure $ fromMaybe defaultStatus status

setAppLockInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, Error TeamFeatureError, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureAppLock ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureAppLock)
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
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureClassifiedDomains)
getClassifiedDomainsInternal _mbtid = do
  globalConfig <- inputs (view (optSettings . setFeatureFlags . flagClassifiedDomains))
  let config = globalConfig
  pure $ case Public.tfwcStatus config of
    Public.TeamFeatureDisabled -> Public.defaultClassifiedDomains
    Public.TeamFeatureEnabled -> config

getConferenceCallingInternal ::
  Members '[BrigAccess, Input Opts, TeamFeatureStore] r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureConferenceCalling)
getConferenceCallingInternal (Left (Just uid)) = do
  getFeatureConfigViaAccount @'Public.TeamFeatureConferenceCalling uid
getConferenceCallingInternal (Left Nothing) = do
  getFeatureStatusWithDefaultConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureConferenceCalling flagConferenceCalling Nothing
getConferenceCallingInternal (Right tid) = do
  getFeatureStatusWithDefaultConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureConferenceCalling flagConferenceCalling (Just tid)

setConferenceCallingInternal ::
  Members '[GundeckAccess, TeamFeatureStore, TeamStore, P.TinyLog] r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureConferenceCalling ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureConferenceCalling)
setConferenceCallingInternal =
  setFeatureStatusNoConfig @'Public.WithoutPaymentStatus @'Public.TeamFeatureConferenceCalling $ \_status _tid -> pure ()

getSelfDeletingMessagesInternal ::
  Member TeamFeatureStore r =>
  GetFeatureInternalParam ->
  Sem r (Public.TeamFeatureStatus 'Public.WithPaymentStatus 'Public.TeamFeatureSelfDeletingMessages)
getSelfDeletingMessagesInternal = \case
  Left _ -> pure Public.defaultSelfDeletingMessagesStatus
  Right tid -> do
    (maybeFeatureStatus, maybePaymentStatus) <- TeamFeatures.getSelfDeletingMessagesStatus tid
    pure $ case (maybePaymentStatus, maybeFeatureStatus) of
      (Just Public.PaymentUnlocked, Just featureStatus) ->
        Public.TeamFeatureStatusWithConfigAndPaymentStatus
          (Public.tfwcStatus featureStatus)
          (Public.tfwcConfig featureStatus)
          Public.PaymentUnlocked
      (Just Public.PaymentUnlocked, Nothing) ->
        Public.TeamFeatureStatusWithConfigAndPaymentStatus
          (Public.tfwcapsStatus Public.defaultSelfDeletingMessagesStatus)
          (Public.tfwcapsConfig Public.defaultSelfDeletingMessagesStatus)
          Public.PaymentUnlocked
      _ -> Public.defaultSelfDeletingMessagesStatus

setSelfDeletingMessagesInternal ::
  Members
    '[ GundeckAccess,
       TeamStore,
       TeamFeatureStore,
       P.TinyLog,
       Error TeamFeatureError
     ]
    r =>
  TeamId ->
  Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSelfDeletingMessages ->
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus 'Public.TeamFeatureSelfDeletingMessages)
setSelfDeletingMessagesInternal tid st = do
  maybePaymentStatus <- TeamFeatures.getPaymentStatus @'Public.TeamFeatureSelfDeletingMessages tid
  case maybePaymentStatus of
    Just (Public.PaymentStatus Public.PaymentUnlocked) -> do
      let pushEvent =
            pushFeatureConfigEvent tid $
              Event.Event Event.Update Public.TeamFeatureSelfDeletingMessages (EdFeatureSelfDeletingMessagesChanged st)
      TeamFeatures.setSelfDeletingMessagesStatus tid st <* pushEvent
    _ -> throw PaymentStatusLocked

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
  Sem r (Public.TeamFeatureStatus 'Public.WithoutPaymentStatus flag)
getFeatureConfigViaAccount = getAccountFeatureConfigClient
