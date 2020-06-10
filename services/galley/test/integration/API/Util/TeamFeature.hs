module API.Util.TeamFeature where

import qualified API.Util as Util
import API.Util (zUser)
import Bilge
import Control.Lens ((.~), view)
import Data.ByteString.Conversion (toByteString')
import Data.Id (TeamId, UserId)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import qualified Network.Wai.Test as WaiTest
import TestSetup
import qualified Wire.API.Team.Feature as Public

withCustomSearchFeature :: FeatureTeamSearchVisibility -> WaiTest.Session () -> TestM ()
withCustomSearchFeature flag action = do
  opts <- view tsGConf
  let opts' = opts & optSettings . setFeatureFlags . flagTeamSearchVisibility .~ flag
  Util.withSettingsOverrides opts' action

getTeamSearchVisibilityAvailable :: HasCallStack => (Request -> Request) -> UserId -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailable = getTeamFeatureFlagWithGalley Public.TeamFeatureSearchVisibility

getTeamSearchVisibilityAvailableInternal :: HasCallStack => (Request -> Request) -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailableInternal =
  getTeamFeatureFlagInternalWithGalley Public.TeamFeatureSearchVisibility

putTeamSearchVisibilityAvailableInternal :: HasCallStack => (Request -> Request) -> TeamId -> Public.TeamFeatureStatus -> (MonadIO m, MonadHttp m) => m ()
putTeamSearchVisibilityAvailableInternal g =
  putTeamFeatureFlagInternalWithGalleyAndMod Public.TeamFeatureSearchVisibility g expect2xx

putLegalHoldEnabledInternal' :: HasCallStack => (Request -> Request) -> TeamId -> Public.TeamFeatureStatus -> TestM ()
putLegalHoldEnabledInternal' = putTeamFeatureFlagInternal' Public.TeamFeatureLegalHold

putTeamFeatureFlagInternal' :: HasCallStack => Public.TeamFeatureName -> (Request -> Request) -> TeamId -> Public.TeamFeatureStatus -> TestM ()
putTeamFeatureFlagInternal' feature reqmod tid status = do
  g <- view tsGalley
  putTeamFeatureFlagInternalWithGalleyAndMod feature g reqmod tid status

putTeamFeatureFlagInternalWithGalleyAndMod ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  Public.TeamFeatureName ->
  (Request -> Request) ->
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatus ->
  m ()
putTeamFeatureFlagInternalWithGalleyAndMod feature galley reqmod tid status =
  void . put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", toByteString' feature]
      . json status
      . reqmod

getTeamFeatureFlagInternal :: HasCallStack => Public.TeamFeatureName -> TeamId -> TestM ResponseLBS
getTeamFeatureFlagInternal feature tid = do
  g <- view tsGalley
  getTeamFeatureFlagInternalWithGalley feature g tid

getTeamFeatureFlagInternalWithGalley :: (MonadIO m, MonadHttp m, HasCallStack) => Public.TeamFeatureName -> (Request -> Request) -> HasCallStack => TeamId -> m ResponseLBS
getTeamFeatureFlagInternalWithGalley feature g tid = do
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", toByteString' feature]

getTeamFeatureFlag :: HasCallStack => Public.TeamFeatureName -> UserId -> TeamId -> TestM ResponseLBS
getTeamFeatureFlag feature uid tid = do
  g <- view tsGalley
  getTeamFeatureFlagWithGalley feature g uid tid

getTeamFeatureFlagWithGalley :: (MonadIO m, MonadHttp m, HasCallStack) => Public.TeamFeatureName -> (Request -> Request) -> UserId -> TeamId -> m ResponseLBS
getTeamFeatureFlagWithGalley feature galley uid tid = do
  get $
    galley
      . paths ["teams", toByteString' tid, "features", toByteString' feature]
      . zUser uid
