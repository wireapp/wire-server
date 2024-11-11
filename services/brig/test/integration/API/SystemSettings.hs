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

module API.SystemSettings (tests) where

import Bilge
import Bilge.Assert
import Control.Lens
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.String.Conversions
import Imports
import Network.Wai.Test as WaiTest
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Version
import Wire.API.SystemSettings
import Wire.ServerOptions.Brig

tests :: Opts -> Manager -> IO TestTree
tests opts m = pure $ do
  testGroup
    "settings"
    [ test m "GET /system/settings/unauthorized" $ testGetSettings opts,
      test m "GET /system/settings" $ testGetSettingsInternal opts
    ]

testGetSettings :: Opts -> Http ()
testGetSettings opts = liftIO $ do
  expectResultForSetting Nothing False
  expectResultForSetting (Just False) False
  expectResultForSetting (Just True) True
  where
    expectResultForSetting :: Maybe Bool -> Bool -> IO ()
    expectResultForSetting restrictUserCreationSetting expectedRes = do
      let newOpts = opts & (settingsLens . restrictUserCreationLens) .~ restrictUserCreationSetting
      -- Run call in `WaiTest.Session` with an adjusted brig `Application`. I.e.
      -- the response is created by running the brig `Application` (with
      -- modified options) directly on the `Request`. No real HTTP request is
      -- made. This happens due to the `MonadHttp WaiTest.Session` instance.
      queriedSettings <- withSettingsOverrides newOpts $ getSystemSettings
      liftIO $
        queriedSettings @?= SystemSettingsPublic expectedRes

    getSystemSettings :: WaiTest.Session SystemSettingsPublic
    getSystemSettings =
      responseJsonError
        =<< get (path (BS.pack ("/" ++ cs latestVersion ++ "/system/settings/unauthorized")))
          <!! statusCode === const 200

testGetSettingsInternal :: Opts -> Http ()
testGetSettingsInternal opts = liftIO $ do
  uid <- randomId
  expectResultForEnableMls uid Nothing False
  expectResultForEnableMls uid (Just False) False
  expectResultForEnableMls uid (Just True) True
  where
    expectResultForEnableMls :: UserId -> Maybe Bool -> Bool -> IO ()
    expectResultForEnableMls uid setEnableMlsValue expectedRes = do
      let newOpts = opts & (settingsLens . enableMLSLens) .~ setEnableMlsValue
      -- Run call in `WaiTest.Session` with an adjusted brig `Application`. I.e.
      -- the response is created by running the brig `Application` (with
      -- modified options) directly on the `Request`. No real HTTP request is
      -- made. This happens due to the `MonadHttp WaiTest.Session` instance.
      queriedSettings <- withSettingsOverrides newOpts $ getSystemSettings uid
      liftIO $ ssInternal queriedSettings @?= SystemSettingsInternal expectedRes

    getSystemSettings :: UserId -> WaiTest.Session SystemSettings
    getSystemSettings uid =
      responseJsonError =<< get (paths (latestVersion : ["system", "settings"]) . zUser uid) <!! statusCode === const 200

latestVersion :: ByteString
latestVersion = toByteString' (maxBound :: Version)
