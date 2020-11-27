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
    getAppLockInternal,
    setAppLockInternal,
    DoAuth (..),
  )
where

import Brig.Types.Team (TeamSize (..))
import Control.Lens
import Control.Monad.Catch
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion hiding (fromList)
import Data.Id
import Data.Range as Range
import Data.String.Conversions (cs)
import Galley.API.Error as Galley
import Galley.API.LegalHold
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Data.SearchVisibility as SearchVisibilityData
import qualified Galley.Data.TeamFeatures as TeamFeatures
import qualified Galley.Intra.Team as BrigTeam
import Galley.Options
import Galley.Types.Teams hiding (newTeam)
import Imports
import Network.Wai
import Network.Wai.Predicate hiding (or, result, setStatus)
import Network.Wai.Utilities
import Wire.API.Team.Feature (featureStatus)
import qualified Wire.API.Team.Feature as Public

data DoAuth = DoAuth UserId | DontDoAuth

getFeatureStatus ::
  forall (a :: Public.TeamFeatureName).
  Public.KnownTeamFeatureName a =>
  (TeamId -> Galley (Public.TeamFeatureStatus a)) ->
  DoAuth ->
  TeamId ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatus getter doauth tid = do
  case doauth of
    DoAuth uid -> do
      zusrMembership <- Data.teamMember tid uid
      void $ permissionCheck (ViewTeamFeature (Public.knownTeamFeatureName @a)) zusrMembership
    DontDoAuth ->
      assertTeamExists tid
  getter tid

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

getAllFeaturesH :: UserId ::: TeamId ::: JSON -> Galley Response
getAllFeaturesH (uid ::: tid ::: _) =
  json <$> getAllFeatures uid tid

getAllFeatures :: UserId -> TeamId -> Galley Aeson.Value
getAllFeatures uid tid = do
  Aeson.object
    <$> sequence
      [ getStatus @'Public.TeamFeatureSSO getSSOStatusInternal,
        getStatus @'Public.TeamFeatureLegalHold getLegalholdStatusInternal,
        getStatus @'Public.TeamFeatureSearchVisibility getTeamSearchVisibilityAvailableInternal,
        getStatus @'Public.TeamFeatureValidateSAMLEmails getValidateSAMLEmailsInternal,
        getStatus @'Public.TeamFeatureDigitalSignatures getDigitalSignaturesInternal,
        getStatus @'Public.TeamFeatureAppLock getAppLockInternal
      ]
  where
    getStatus ::
      forall (a :: Public.TeamFeatureName).
      ( Public.KnownTeamFeatureName a,
        Aeson.ToJSON (Public.TeamFeatureStatus a)
      ) =>
      (TeamId -> Galley (Public.TeamFeatureStatus a)) ->
      Galley (Text, Aeson.Value)
    getStatus getter = do
      status <- getFeatureStatus @a getter (DoAuth uid) tid
      let feature = Public.knownTeamFeatureName @a
      pure $ (cs (toByteString' feature) Aeson..= status)

getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName).
  (Public.KnownTeamFeatureName a, Public.FeatureHasNoConfig a) =>
  Galley Public.TeamFeatureStatusValue ->
  TeamId ->
  Galley (Public.TeamFeatureStatus a)
getFeatureStatusNoConfig getDefault tid = do
  defaultStatus <- Public.TeamFeatureStatusNoConfig <$> getDefault
  fromMaybe defaultStatus <$> TeamFeatures.getFeatureStatusNoConfig @a tid

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName).
  (Public.KnownTeamFeatureName a, Public.FeatureHasNoConfig a) =>
  (Public.TeamFeatureStatusValue -> TeamId -> Galley ()) ->
  TeamId ->
  Public.TeamFeatureStatus a ->
  Galley (Public.TeamFeatureStatus a)
setFeatureStatusNoConfig applyState tid status = do
  applyState (featureStatus status) tid
  TeamFeatures.setFeatureStatusNoConfig @a tid status

getSSOStatusInternal :: TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSSO)
getSSOStatusInternal = getFeatureStatusNoConfig @'Public.TeamFeatureSSO $ do
  view (options . optSettings . setFeatureFlags . flagSSO) <&> \case
    FeatureSSOEnabledByDefault -> Public.TeamFeatureEnabled
    FeatureSSODisabledByDefault -> Public.TeamFeatureDisabled

setSSOStatusInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureSSO) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSSO)
setSSOStatusInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSSO $ \case
  Public.TeamFeatureDisabled -> const (throwM disableSsoNotImplemented)
  Public.TeamFeatureEnabled -> const (pure ())

getTeamSearchVisibilityAvailableInternal :: TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
getTeamSearchVisibilityAvailableInternal = getFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility $ do
  view (options . optSettings . setFeatureFlags . flagTeamSearchVisibility) <&> \case
    FeatureTeamSearchVisibilityEnabledByDefault -> Public.TeamFeatureEnabled
    FeatureTeamSearchVisibilityDisabledByDefault -> Public.TeamFeatureDisabled

setTeamSearchVisibilityAvailableInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureSearchVisibility)
setTeamSearchVisibilityAvailableInternal = setFeatureStatusNoConfig @'Public.TeamFeatureSearchVisibility $ \case
  Public.TeamFeatureDisabled -> SearchVisibilityData.resetSearchVisibility
  Public.TeamFeatureEnabled -> const (pure ())

getValidateSAMLEmailsInternal :: TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails)
getValidateSAMLEmailsInternal =
  -- FUTUREWORK: we may also want to get a default from the server config file here, like for
  -- sso, and team search visibility.
  getFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails $
    pure Public.TeamFeatureDisabled

setValidateSAMLEmailsInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureValidateSAMLEmails)
setValidateSAMLEmailsInternal = setFeatureStatusNoConfig @'Public.TeamFeatureValidateSAMLEmails $ \_ _ -> pure ()

getDigitalSignaturesInternal :: TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures)
getDigitalSignaturesInternal =
  -- FUTUREWORK: we may also want to get a default from the server config file here, like for
  -- sso, and team search visibility.
  getFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures $ do
    pure Public.TeamFeatureDisabled

setDigitalSignaturesInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureDigitalSignatures)
setDigitalSignaturesInternal = setFeatureStatusNoConfig @'Public.TeamFeatureDigitalSignatures $ \_ _ -> pure ()

getLegalholdStatusInternal :: TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold)
getLegalholdStatusInternal tid = do
  featureLegalHold <- view (options . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      let defaultStatus = Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled
      status <- TeamFeatures.getFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid
      pure (fromMaybe defaultStatus status)
    FeatureLegalHoldDisabledPermanently -> do
      pure (Public.TeamFeatureStatusNoConfig Public.TeamFeatureDisabled)

setLegalholdStatusInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureLegalHold)
setLegalholdStatusInternal tid status = do
  do
    featureLegalHold <- view (options . optSettings . setFeatureFlags . flagLegalHold)
    case featureLegalHold of
      FeatureLegalHoldDisabledByDefault -> do
        pure ()
      FeatureLegalHoldDisabledPermanently -> do
        throwM legalHoldFeatureFlagNotEnabled
  case featureStatus status of
    Public.TeamFeatureDisabled -> removeSettings' tid
    -- FUTUREWORK: We cannot enable legalhold on large teams right now
    Public.TeamFeatureEnabled -> checkTeamSize
  TeamFeatures.setFeatureStatusNoConfig @'Public.TeamFeatureLegalHold tid status
  where
    checkTeamSize = do
      (TeamSize size) <- BrigTeam.getSize tid
      limit <- fromIntegral . fromRange <$> fanoutLimit
      when (size > limit) $ do
        throwM cannotEnableLegalHoldServiceLargeTeam

getAppLockInternal :: TeamId -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock)
getAppLockInternal tid = do
  Defaults defaultStatus <- view (options . optSettings . setFeatureFlags . flagAppLockDefaults)
  status <- TeamFeatures.getApplockFeatureStatus tid
  pure $ fromMaybe defaultStatus status

setAppLockInternal :: TeamId -> (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock) -> Galley (Public.TeamFeatureStatus 'Public.TeamFeatureAppLock)
setAppLockInternal tid status = do
  when (Public.applockInactivityTimeoutSecs (Public.tfwcConfig status) < 30) $
    throwM inactivityTimeoutTooLow
  TeamFeatures.setApplockFeatureStatus tid status
