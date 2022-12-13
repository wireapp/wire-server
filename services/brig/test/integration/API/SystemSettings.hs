module API.SystemSettings (tests) where

import Bilge
import Bilge.Assert
import Brig.Options
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Imports
import Network.Wai.Test as WaiTest
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Version
import Wire.API.SystemSettings

tests :: Opts -> Manager -> IO TestTree
tests opts m = pure $ do
  testGroup
    "settings"
    [ test m "GET /system/settings" $ testGetSettings opts
    ]

testGetSettings :: Opts -> Http ()
testGetSettings opts = liftIO $ do
  expectResultForSetting Nothing False
  expectResultForSetting (Just False) False
  expectResultForSetting (Just True) True
  where
    expectResultForSetting :: Maybe Bool -> Bool -> IO ()
    expectResultForSetting restrictUserCreationSetting expectedRes = do
      let newOpts = opts & (optionSettings . restrictUserCreation) .~ restrictUserCreationSetting
      queriedSettings <- withSettingsOverrides newOpts $ getSystemSettings
      liftIO $
        queriedSettings @?= SystemSettings expectedRes

getSystemSettings :: WaiTest.Session SystemSettings
getSystemSettings =
  responseJsonError
    =<< get (path (BS.pack ("/" ++ latestVersion ++ "/system/settings/unauthorized")))
      <!! statusCode
        Bilge.Assert.=== const 200
  where
    latestVersion :: String
    latestVersion = map toLower $ show (maxBound :: Version)
