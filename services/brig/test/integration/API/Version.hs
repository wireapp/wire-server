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

module API.Version (tests) where

import Bilge
import Bilge.Assert
import Brig.Options
import qualified Brig.Options as Opt
import Control.Lens ((?~))
import Control.Monad.Catch (MonadCatch)
import qualified Data.Set as Set
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Version
import Wire.API.User

tests :: Manager -> Opts -> Brig -> TestTree
tests p opts brig =
  testGroup
    "version"
    [ test p "GET /api-version" $ testVersion brig,
      test p "GET /v1/api-version" $ testVersionV1 brig,
      test p "GET /api-version (with dev)" $ testDevVersion opts brig,
      test p "GET /v500/api-version" $ testUnsupportedVersion brig,
      test p "GET /api-version (federation info)" $ testFederationDomain opts brig,
      test p "Disabled version is unsupported" $ testDisabledVersionIsUnsupported opts brig,
      test p "Disabled version is not advertised" $ testVersionDisabledSupportedVersion opts brig,
      test p "Disabled dev version is not advertised" $ testVersionDisabledDevelopmentVersion opts brig
    ]

testVersion :: Brig -> Http ()
testVersion brig = do
  vinfo <-
    responseJsonError
      =<< get (brig . path "/api-version")
        <!! const 200 === statusCode
  liftIO $
    (fromVersionNumber <$> vinfoSupported vinfo) @?= supportedVersions \\ developmentVersions

testVersionV1 :: Brig -> Http ()
testVersionV1 brig = do
  vinfo <-
    responseJsonError
      =<< get (apiVersion "v1" . brig . path "api-version")
        <!! const 200 === statusCode
  liftIO $
    (fromVersionNumber <$> vinfoSupported vinfo) @?= supportedVersions \\ developmentVersions

testDevVersion :: Opts -> Brig -> Http ()
testDevVersion opts brig = withSettingsOverrides
  (opts & optionSettings . enableDevelopmentVersions ?~ True)
  $ do
    vinfo <-
      responseJsonError
        =<< get (brig . path "/api-version")
          <!! const 200 === statusCode
    liftIO $
      (fromVersionNumber <$> vinfoSupported vinfo) @?= supportedVersions

testUnsupportedVersion :: Brig -> Http ()
testUnsupportedVersion brig = do
  e <-
    responseJsonError
      =<< get (apiVersion "v500" . brig . path "api-version")
        <!! const 404 === statusCode
  liftIO $ Wai.label e @?= "unsupported-version"

testFederationDomain :: Opts -> Brig -> Http ()
testFederationDomain opts brig = do
  let domain = setFederationDomain (optSettings opts)
  vinfo <-
    responseJsonError
      =<< get (brig . path "/api-version")
        <!! const 200 === statusCode
  liftIO $ do
    vinfoFederation vinfo @?= True
    vinfoDomain vinfo @?= domain

testDisabledVersionIsUnsupported :: Opts -> Brig -> Http ()
testDisabledVersionIsUnsupported opts brig = do
  uid <- userId <$> randomUser brig

  get (apiVersion "v2" . brig . path "/self" . zUser uid)
    !!! const 200 === statusCode

  withSettingsOverrides
    ( opts
        & Opt.optionSettings
          . Opt.disabledAPIVersions
          ?~ Set.fromList [V2]
    )
    $ do
      err <-
        responseJsonError
          =<< get (apiVersion "v2" . brig . path "/self" . zUser uid)
            <!! const 404 === statusCode
      liftIO $
        Wai.label err @?= "unsupported-version"

      get (apiVersion "v1" . brig . path "/self" . zUser uid)
        !!! const 200 === statusCode

      get (apiVersion "v3" . brig . path "/self" . zUser uid)
        !!! const 200 === statusCode

      get (unversioned . brig . path "/self" . zUser uid)
        !!! const 200 === statusCode

testVersionDisabledSupportedVersion :: Opts -> Brig -> Http ()
testVersionDisabledSupportedVersion opts brig = do
  vinfo <- getVersionInfo brig
  liftIO $ filter (== VersionNumber V2) (vinfoSupported vinfo) @?= [VersionNumber V2]
  disabledVersionIsNotAdvertised opts brig V2

testVersionDisabledDevelopmentVersion :: Opts -> Brig -> Http ()
testVersionDisabledDevelopmentVersion opts brig = do
  vinfo <- getVersionInfo brig
  for_ (listToMaybe (vinfoDevelopment vinfo)) $ \devVersion -> do
    liftIO $ filter (== devVersion) (vinfoDevelopment vinfo) @?= [devVersion]
    disabledVersionIsNotAdvertised opts brig (fromVersionNumber devVersion)

disabledVersionIsNotAdvertised :: Opts -> Brig -> Version -> Http ()
disabledVersionIsNotAdvertised opts brig version =
  withSettingsOverrides
    ( opts
        & Opt.optionSettings
          . Opt.disabledAPIVersions
          ?~ Set.fromList [version]
    )
    $ do
      vinfo <- getVersionInfo brig
      liftIO $ filter (== VersionNumber version) (vinfoSupported vinfo) @?= []
      liftIO $ filter (== VersionNumber version) (vinfoDevelopment vinfo) @?= []

getVersionInfo ::
  (MonadIO m, MonadCatch m, MonadHttp m, HasCallStack) =>
  Brig ->
  m VersionInfo
getVersionInfo brig =
  responseJsonError
    =<< get (unversioned . brig . path "/api-version")
      <!! const 200 === statusCode
