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

module API.Util.TeamFeature where

import API.Util (zUser)
import qualified API.Util as Util
import Bilge
import qualified Bilge.TestSession as BilgeTest
import Control.Lens (view, (.~))
import Data.Aeson (ToJSON)
import Data.ByteString.Conversion (toByteString')
import Data.Id (TeamId, UserId)
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types.Teams
import Imports
import TestSetup
import qualified Wire.API.Team.Feature as Public

withCustomSearchFeature :: FeatureTeamSearchVisibility -> BilgeTest.SessionT TestM () -> TestM ()
withCustomSearchFeature flag action = do
  opts <- view tsGConf
  let opts' = opts & optSettings . setFeatureFlags . flagTeamSearchVisibility .~ flag
  Util.withSettingsOverrides opts' action

getTeamSearchVisibilityAvailable :: HasCallStack => (Request -> Request) -> UserId -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailable = getTeamFeatureFlagWithGalley Public.TeamFeatureSearchVisibility

getTeamSearchVisibilityAvailableInternal :: HasCallStack => (Request -> Request) -> TeamId -> (MonadIO m, MonadHttp m) => m ResponseLBS
getTeamSearchVisibilityAvailableInternal =
  getTeamFeatureFlagInternalWithGalley Public.TeamFeatureSearchVisibility

putTeamSearchVisibilityAvailableInternal ::
  HasCallStack =>
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatusValue ->
  (MonadIO m, MonadHttp m) => m ()
putTeamSearchVisibilityAvailableInternal g tid statusValue =
  putTeamFeatureFlagInternalWithGalleyAndMod
    @'Public.TeamFeatureSearchVisibility
    g
    expect2xx
    tid
    (Public.TeamFeatureStatusNoConfig statusValue)

putLegalHoldEnabledInternal' ::
  HasCallStack =>
  (Request -> Request) ->
  TeamId ->
  Public.TeamFeatureStatusValue ->
  TestM ()
putLegalHoldEnabledInternal' g tid statusValue =
  putTeamFeatureFlagInternal @'Public.TeamFeatureLegalHold g tid (Public.TeamFeatureStatusNoConfig statusValue)

--------------------------------------------------------------------------------

getTeamFeatureFlagInternal ::
  (HasCallStack) =>
  Public.TeamFeatureName ->
  TeamId ->
  TestM ResponseLBS
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

putTeamFeatureFlagInternal ::
  forall (a :: Public.TeamFeatureName).
  ( HasCallStack,
    Public.KnownTeamFeatureName a,
    ToJSON (Public.TeamFeatureStatus a)
  ) =>
  (Request -> Request) ->
  TeamId ->
  (Public.TeamFeatureStatus a) ->
  TestM ()
putTeamFeatureFlagInternal reqmod tid status = do
  g <- view tsGalley
  putTeamFeatureFlagInternalWithGalleyAndMod @a g reqmod tid status

putTeamFeatureFlagInternalWithGalleyAndMod ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadIO m,
    MonadHttp m,
    HasCallStack,
    Public.KnownTeamFeatureName a,
    ToJSON (Public.TeamFeatureStatus a)
  ) =>
  (Request -> Request) ->
  (Request -> Request) ->
  TeamId ->
  (Public.TeamFeatureStatus a) ->
  m ()
putTeamFeatureFlagInternalWithGalleyAndMod galley reqmod tid status =
  void . put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", toByteString' (Public.knownTeamFeatureName @a)]
      . json status
      . reqmod
