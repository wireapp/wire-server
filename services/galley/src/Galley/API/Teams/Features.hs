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
    getFeatureStatusPersonal,
    getFeatureStatusTeam,
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
    DoAuth (..),
  )
where

import Control.Lens
import Control.Monad.Catch
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion hiding (fromList)
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.String.Conversions (cs)
import Galley.API.Error as Galley
import Galley.API.LegalHold
import Galley.API.Teams (ensureNotTooLargeToActivateLegalHold)
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.SearchVisibility as SearchVisibilityData
import qualified Galley.Data.TeamFeatures as TeamFeatures
import Galley.Intra.Push (PushEvent (FeatureConfigEvent), newPush, push1)
import Galley.Options
import Galley.Types.Teams hiding (newTeam)
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (Error, or, result, setStatus)
import Network.Wai.Utilities
import qualified System.Logger.Class as Log
import Wire.API.Event.FeatureConfig (EventData (EdFeatureWithoutConfigChanged))
import qualified Wire.API.Event.FeatureConfig as Event
import Wire.API.Team.Feature (AllFeatureConfigs (..), FeatureHasNoConfig, KnownTeamFeatureName, TeamFeatureName)
import qualified Wire.API.Team.Feature as Public

data DoAuth = DoAuth UserId | DontDoAuth

getFeatureStatus ::
  forall (a :: Public.TeamFeatureName).
  Public.KnownTeamFeatureName a =>
  (Maybe TeamId -> Galley (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  Maybe TeamId ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatus getter _doauth Nothing = getter Nothing
getFeatureStatus getter doauth (Just tid) = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- Data.teamMember tid uid
      void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getter (Just tid)

getFeatureStatusPersonal ::
  forall (a :: Public.TeamFeatureName).
  Public.KnownTeamFeatureName a =>
  (Maybe TeamId -> Galley (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatusPersonal getter doauth = getFeatureStatus @a getter doauth Nothing

getFeatureStatusTeam ::
  forall (a :: Public.TeamFeatureName).
  Public.KnownTeamFeatureName a =>
  (Maybe TeamId -> Galley (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  TeamId ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatusTeam getter doauth tid = getFeatureStatus @a getter doauth (Just tid)

setFeatureStatus ::
  forall (a :: Public.TeamFeatureName).
  Public.KnownTeamFeatureName a =>
  (TeamId -> Public.TeamFeatureStatus a -> Galley (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  TeamId ->
  Public.TeamFeatureStatus a ->
  Galley (Public.TeamFeatureStatus a)
setFeatureStatus setter doauth tid status = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- Data.teamMember tid uid
      void $ permissionCheck (ChangeTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  setter tid status

getAllFeatureConfigs :: UserId -> Galley AllFeatureConfigs
getAllFeatureConfigs zusr = getAllFeatures zusr =<< Data.oneUserTeam zusr

getAllFeaturesH :: UserId ::: TeamId ::: JSON -> Galley Response
getAllFeaturesH (uid ::: tid ::: _) =
  json <$> getAllFeatures uid (Just tid)

getAllFeatures :: UserId -> Maybe TeamId -> Galley AllFeatureConfigs
getAllFeatures uid mbtid = do
  AllFeatureConfigs . HashMap.fromList
    <$> sequence
      [ getStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.TeamFeatureAppLock getAppLockInternal,
        getStatus @'Public.TeamFeatureFileSharing getFileSharingInternal,
        getStatus @'Public.TeamFeatureClassifiedDomains getClassifiedDomainsInternal,
        getStatus @'Public.TeamFeatureConferenceCalling getConferenceCallingInternal
      ]
  where
    getStatus ::
      forall (a :: Public.TeamFeatureName).
      ( Public.KnownTeamFeatureName a,
        Aeson.ToJSON (Public.TeamFeatureStatus a)
      ) =>
      (Maybe TeamId -> Galley (Public.TeamFeatureStatus a)) ->
      Galley (Text, Aeson.Value)
    getStatus getter = do
      status <- getFeatureStatus @a getter (DoAuth uid) mbtid
      let feature = Public.knownTeamFeatureName @a
      pure $ (cs (toByteString' feature) Aeson..= status)

getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName).
  (Public.KnownTeamFeatureName a, Public.FeatureHasNoConfig a, TeamFeatures.HasStatusCol a) =>
  Galley Public.TeamFeatureStatusValue ->
  TeamId ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- Public.TeamFeatureStatusNoConfig <$> getDefault
  fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @a tid

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName).
  (Public.KnownTeamFeatureName a, Public.FeatureHasNoConfig a, TeamFeatures.HasStatusCol a) =>
  (Public.TeamFeatureStatusValue -> TeamId -> Galley ()) ->
  TeamId ->
  Public.TeamFeatureStatus a ->
  Galley (Public.TeamFeatureStatus a)
setFeatureStatusNoConfig applyState tid status = do
  applyState (Public.tfwoStatus status) tid
  newStatus <- TeamFeatures.setFeatureStatusNoConfig @a tid status
  pushFeatureConfigEvent tid $
    Event.Event Event.Update (Public.knownTeamFeatureName @a) (EdFeatureWithoutConfigChanged newStatus)
  pure newStatus

getSSOStatusInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSSO)
getSSOStatusInternal =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureSSO getDef)
  where
    getDef :: Galley Public.TeamFeatureStatusValue
    getDef =
      view (options . optSettings . setFeatureFlags . flagSSO) <&> \case
        FeatureSSOEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureSSODisabledByDefault -> Public.TeamFeatureDisabled

setSSOStatusInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureSSO) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSSO)
setSSOStatusInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSSO $ \case
  Public.TeamFeatureDisabled -> const (throwM disableSsoNotImplemented)
  Public.TeamFeatureEnabled -> const (pure ())

getTeamSearchVisibilityAvailableInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility getDef)
  where
    getDef = do
      view (options . optSettings . setFeatureFlags . flagTeamSearchVisibility) <&> \case
        FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
        FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility $ \case
  Public.TeamFeatureDisabled -> SearchVisibilityData.resetSearchVisibility
  Public.TeamFeatureEnabled -> const (pure ())

getValidateSAMLEmailsInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails)
getValidateSAMLEmailsInternal =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setValidateSAMLEmailsInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails)
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures)
getDigitalSignaturesInternal =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures getDef)
  where
    -- FUTUREWORK: we may also want to get a default from the server config file here, like for
    -- sso, and team search visibility.
    -- Use getFeatureStatusWithDefault
    getDef = pure Public.TeamFeatureDisabled

setDigitalSignaturesInternal :: TeamId -> Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures)
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures $ \_ _ -> pure ()

getLegalholdStatusInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold)
getLegalholdStatusInternal Nothing =
  pure $ Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled
getLegalholdStatusInternal (Just tid) = do
  isLegalHoldEnabledForTeam tid <&> \case
    True -> Public.TeamFeatureStatusNoConfig Public.TeamFeatureEnabled
    False -> Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled

setLegalholdStatusInternal :: TeamId -> Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold)
setLegalholdStatusInternal tid status@(Public.tfwoStatus -> statusValue) = do
  do
    -- this extra do is to encapsulate the assertions running before the actual operation.
    -- enabeling LH for teams is only allowed in normal operation; disabled-permanently and
    -- whitelist-teams have no or their own way to do that, resp.
    featureLegalHold <- view (options . optSettings . setFeatureFlags . flagLegalHold)
    case featureLegalHold of
      FeatureLegalHoldDisabledByDefault -> do
        pure ()
      FeatureLegalHoldDisabledPermanently -> do
        throwM legalHoldFeatureFlagNotEnabled
      FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> do
        throwM legalHoldWhitelistedOnly

  -- we're good to update the status now.
  case statusValue of
    Public.TeamFeatureDisabled -> removeSettings' tid
    Public.TeamFeatureEnabled -> do
      ensureNotTooLargeToActivateLegalHold tid
  TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid status

getFileSharingInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureFileSharing)
getFileSharingInternal =
  getFeatureStatusWithDefaultConfig @'Public.TeamFeatureFileSharing flagFileSharing

getFeatureStatusWithDefaultConfig ::
  forall (a :: TeamFeatureName).
  (KnownTeamFeatureName a, TeamFeatures.HasStatusCol a, FeatureHasNoConfig a) =>
  Lens' FeatureFlags (Defaults (Public.TeamFeatureStatus a)) ->
  Maybe TeamId ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatusWithDefaultConfig lens' =
  maybe
    (Public.TeamFeatureStatusNoConfig <$> getDef)
    (getFeatureStatusNoConfig @a getDef)
  where
    getDef :: Galley Public.TeamFeatureStatusValue
    getDef =
      view (options . optSettings . setFeatureFlags . lens')
        <&> Public.tfwoStatus . view unDefaults

setFileSharingInternal :: TeamId -> Public.TeamFeatureStatus 'Public.TeamFeatureFileSharing -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureFileSharing)
setFileSharingInternal = setFeatureStatusNoConfig @'Public.TeamFeatureFileSharing $ \_status _tid -> pure ()

getAppLockInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock)
getAppLockInternal mbtid = do
  Defaults defaultStatus <- view (options . optSettings . setFeatureFlags . flagAppLockDefaults)
  status <- join <$> (TeamFeatures.getApplockFeatureStatus `mapM` mbtid)
  pure $ fromMaybe defaultStatus status

setAppLockInternal :: TeamId -> Public.TeamFeatureStatus 'Public.TeamFeatureAppLock -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock)
setAppLockInternal tid status = do
  when (Public.applockInactivityTimeoutSecs (Public.tfwcConfig status) < 30) $
    throwM inactivityTimeoutTooLow
  TeamFeatures.setApplockFeatureStatus tid status

getClassifiedDomainsInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureClassifiedDomains)
getClassifiedDomainsInternal _mbtid = do
  globalConfig <- view (options . optSettings . setFeatureFlags . flagClassifiedDomains)
  let config = globalConfig
  pure $ case Public.tfwcStatus config of
    Public.TeamFeatureDisabled ->
      Public.TeamFeatureStatusWithConfig Public.TeamFeatureDisabled (Public.TeamFeatureClassifiedDomainsConfig [])
    Public.TeamFeatureEnabled -> config

getConferenceCallingInternal :: Maybe TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureConferenceCalling)
getConferenceCallingInternal = getFeatureStatusWithDefaultConfig @'Public.TeamFeatureConferenceCalling flagConferenceCalling

setConferenceCallingInternal :: TeamId -> Public.TeamFeatureStatus 'Public.TeamFeatureConferenceCalling -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureConferenceCalling)
setConferenceCallingInternal = setFeatureStatusNoConfig @'Public.TeamFeatureConferenceCalling $ \_status _tid -> pure ()

pushFeatureConfigEvent :: TeamId -> Event.Event -> Galley ()
pushFeatureConfigEvent tid event = do
  memList <- Data.teamMembersForFanout tid
  when ((memList ^. teamMemberListType) == ListTruncated) $ do
    Log.warn $
      Log.field "action" (Log.val "Features.pushFeatureConfigEvent")
        . Log.field "feature" (Log.val (toByteString' . Event._eventFeatureName $ event))
        . Log.field "team" (Log.val (cs . show $ tid))
        . Log.msg @Text "Fanout limit exceeded. Some events will not be sent."
  let recipients = membersToRecipients Nothing (memList ^. teamMembers)
  for_
    (newPush (memList ^. teamMemberListType) Nothing (FeatureConfigEvent event) recipients)
    push1
