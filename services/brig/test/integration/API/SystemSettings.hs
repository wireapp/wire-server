module API.SystemSettings (tests) where

import Bilge
import Bilge.Assert
import Brig.Options
import Control.Lens
import Control.Monad.Catch
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.SystemSettings

tests :: Opts -> Manager -> Brig -> IO TestTree
tests opts m brig = pure $ do
  testGroup
    "settings"
    [ test m "GET /system/settings" $ testGetSettings opts brig
    ]

testGetSettings :: Opts -> Brig -> Http ()
testGetSettings opts brig = liftIO $ do
  expectResultForSetting Nothing False
  expectResultForSetting (Just False) False
  expectResultForSetting (Just True) True
  where
    expectResultForSetting :: Maybe Bool -> Bool -> IO ()
    expectResultForSetting s expectedRes = do
      let newOpts = opts & (optionSettings . restrictUserCreation) .~ s
      queriedSettings <- withSettingsOverrides newOpts $ getSystemSettings brig
      liftIO $
        systemSettingsSetRestrictUserCreation queriedSettings @?= expectedRes

getSystemSettings ::
  (HasCallStack, MonadIO m, MonadHttp m, MonadCatch m, MonadThrow m) =>
  Brig ->
  m SystemSettings
getSystemSettings brig =
  responseJsonError
    =<< get (brig . path "/system/settings")
      <!! statusCode
        Bilge.Assert.=== const 200
