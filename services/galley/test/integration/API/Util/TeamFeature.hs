-- Disabling to stop warnings on HasCallStack
{-# LANGUAGE DeepSubsumption #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module API.Util.TeamFeature where

import API.Util (HasGalley (viewGalley), zUser)
import API.Util qualified as Util
import Bilge
import Control.Lens ((.~), (^?))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, Result (Success), ToJSON, Value, fromJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Lens
import Data.ByteString.Conversion (toByteString')
import Data.Id (ConvId, TeamId, UserId)
import Data.Schema
import GHC.TypeLits (KnownSymbol)
import Galley.Options (featureFlags, settings)
import Galley.Types.Teams
import Imports
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))
import TestSetup
import Wire.API.Team.Feature
import Wire.API.Team.Feature qualified as Public

withCustomSearchFeature :: FeatureTeamSearchVisibilityAvailability -> TestM () -> TestM ()
withCustomSearchFeature flag action = do
  Util.withSettingsOverrides (\opts -> opts & settings . featureFlags . flagTeamSearchVisibility .~ flag) action

putTeamSearchVisibilityAvailableInternal ::
  HasCallStack =>
  TeamId ->
  Public.FeatureStatus ->
  (MonadIO m, MonadHttp m, HasGalley m) => m ()
putTeamSearchVisibilityAvailableInternal tid statusValue =
  void $
    putTeamFeatureInternal
      @Public.SearchVisibilityAvailableConfig
      expect2xx
      tid
      (Public.WithStatusNoLock statusValue Public.SearchVisibilityAvailableConfig Public.FeatureTTLUnlimited)

getTeamFeatureInternal ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, KnownSymbol (Public.FeatureSymbol cfg)) =>
  TeamId ->
  m ResponseLBS
getTeamFeatureInternal tid = do
  g <- viewGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]

getTeamFeature ::
  forall cfg m.
  (HasGalley m, MonadIO m, MonadHttp m, HasCallStack, KnownSymbol (Public.FeatureSymbol cfg)) =>
  UserId ->
  TeamId ->
  m ResponseLBS
getTeamFeature uid tid = do
  galley <- viewGalley
  get $
    galley
      . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . zUser uid

getAllTeamFeatures ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m) =>
  UserId ->
  TeamId ->
  m ResponseLBS
getAllTeamFeatures uid tid = do
  g <- viewGalley
  get $
    g
      . paths ["teams", toByteString' tid, "features"]
      . zUser uid

getTeamFeatureFromAll ::
  forall cfg m.
  ( HasCallStack,
    MonadThrow m,
    HasGalley m,
    MonadIO m,
    MonadHttp m,
    KnownSymbol (Public.FeatureSymbol cfg),
    FromJSON (Public.WithStatus cfg)
  ) =>
  UserId ->
  TeamId ->
  m (Public.WithStatus cfg)
getTeamFeatureFromAll uid tid = do
  response :: Value <- responseJsonError =<< getAllTeamFeatures uid tid
  let status = response ^? key (Key.fromText (Public.featureName @cfg))
  maybe (error "getting all features failed") pure (status >>= fromResult . fromJSON)
  where
    fromResult :: Result a -> Maybe a
    fromResult (Success b) = Just b
    fromResult _ = Nothing

getAllFeatureConfigs ::
  (HasCallStack, HasGalley m, Monad m, MonadHttp m) =>
  UserId ->
  m ResponseLBS
getAllFeatureConfigs uid = do
  g <- viewGalley
  get $
    g
      . paths ["feature-configs"]
      . zUser uid

getFeatureConfig ::
  forall cfg m.
  ( HasCallStack,
    MonadThrow m,
    HasGalley m,
    MonadHttp m,
    KnownSymbol (Public.FeatureSymbol cfg),
    FromJSON (Public.WithStatus cfg)
  ) =>
  UserId ->
  m (Public.WithStatus cfg)
getFeatureConfig uid = do
  response :: Value <- responseJsonError =<< getAllFeatureConfigs uid
  let status = response ^? key (Key.fromText (Public.featureName @cfg))
  maybe (error "getting all feature configs failed") pure (status >>= fromResult . fromJSON)
  where
    fromResult :: Result a -> Maybe a
    fromResult (Success b) = Just b
    fromResult _ = Nothing

putTeamFeature ::
  forall cfg.
  ( HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  UserId ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  TestM ResponseLBS
putTeamFeature uid tid status = do
  galley <- viewGalley
  put $
    galley
      . paths ["teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json status
      . zUser uid

putTeamFeatureInternal ::
  forall cfg m.
  ( Monad m,
    HasGalley m,
    MonadHttp m,
    HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToJSON (Public.WithStatusNoLock cfg)
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.WithStatusNoLock cfg ->
  m ResponseLBS
putTeamFeatureInternal reqmod tid status = do
  galley <- viewGalley
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json status
      . reqmod

setLockStatusInternal ::
  forall cfg.
  ( HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg)
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.LockStatus ->
  TestM ResponseLBS
setLockStatusInternal reqmod tid lockStatus = do
  galley <- viewGalley
  put $
    galley
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg, toByteString' lockStatus]
      . reqmod

patchTeamFeatureInternal ::
  forall cfg.
  ( HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToSchema cfg
  ) =>
  TeamId ->
  Public.WithStatusPatch cfg ->
  TestM ResponseLBS
patchTeamFeatureInternal = patchTeamFeatureInternalWithMod id

patchTeamFeatureInternalWithMod ::
  forall cfg.
  ( HasCallStack,
    KnownSymbol (Public.FeatureSymbol cfg),
    ToSchema cfg
  ) =>
  (Request -> Request) ->
  TeamId ->
  Public.WithStatusPatch cfg ->
  TestM ResponseLBS
patchTeamFeatureInternalWithMod reqmod tid reqBody = do
  galley <- viewGalley
  patch $
    galley
      . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @cfg]
      . json reqBody
      . reqmod

getGuestLinkStatus ::
  HasCallStack =>
  (Request -> Request) ->
  UserId ->
  ConvId ->
  TestM ResponseLBS
getGuestLinkStatus galley u cid =
  get $
    galley
      . paths ["conversations", toByteString' cid, "features", Public.featureNameBS @Public.GuestLinksConfig]
      . zUser u

checkTeamFeatureAllEndpoints ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    ToSchema cfg,
    Typeable cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg)
  ) =>
  UserId ->
  TeamId ->
  WithStatus cfg ->
  TestM ()
checkTeamFeatureAllEndpoints uid tid expected = do
  compareLeniently $ responseJsonUnsafe <$> getTeamFeatureInternal @cfg tid
  compareLeniently $ responseJsonUnsafe <$> getTeamFeature @cfg uid tid
  compareLeniently $ getTeamFeatureFromAll @cfg uid tid
  compareLeniently $ getFeatureConfig uid
  where
    compareLeniently :: TestM (WithStatus cfg) -> TestM ()
    compareLeniently receive = do
      received <- receive
      liftIO $ do
        wsStatus received @?= wsStatus expected
        wsLockStatus received @?= wsLockStatus expected
        wsConfig received @?= wsConfig expected
        checkTtl (wsTTL received) (wsTTL expected)

    checkTtl :: FeatureTTL -> FeatureTTL -> IO ()
    checkTtl (FeatureTTLSeconds actualTtl) (FeatureTTLSeconds expectedTtl) =
      assertBool
        ("expected the actual TTL to be greater than 0 and equal to or no more than 2 seconds less than " <> show expectedTtl <> ", but it was " <> show actualTtl)
        ( actualTtl > 0
            && actualTtl <= expectedTtl
            && abs (fromIntegral @Word @Int actualTtl - fromIntegral @Word @Int expectedTtl) <= 2
        )
    checkTtl FeatureTTLUnlimited FeatureTTLUnlimited = pure ()
    checkTtl FeatureTTLUnlimited _ = assertFailure "expected the actual TTL to be unlimited, but it was limited"
    checkTtl _ FeatureTTLUnlimited = assertFailure "expected the actual TTL to be limited, but it was unlimited"
