module API.SystemSettings (tests) where

import Bilge
import Bilge.Assert
import Brig.Options
import Control.Lens.Operators
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
    [ test m "GET /system/settings - empty " $ testGetSettings opts brig testSettingsEmpty,
      test m "GET /system/settings - set" $ testGetSettings opts brig testSettingsSet,
      test m "GET /system/settings - unset" $ testGetSettings opts brig testSettingsUnset
    ]

testSettingsEmpty :: SystemSettings
testSettingsEmpty =
  SystemSettings
    {
      systemSettingsSetRestrictUserCreation = Nothing
    }

testSettingsSet :: SystemSettings
testSettingsSet =
  SystemSettings
    {
      systemSettingsSetRestrictUserCreation = Just True
    }

testSettingsUnset :: SystemSettings
testSettingsUnset =
  SystemSettings
    {
      systemSettingsSetRestrictUserCreation = Just True
    }

testGetSettings :: Opts -> Brig -> SystemSettings -> Http ()
testGetSettings opts brig testSettings = do
  let newSettings =
        (opts & optSettings)
          {
            setRestrictUserCreation = systemSettingsSetRestrictUserCreation testSettings
          }
  let newOpts =
        opts
          { optSettings = newSettings
          }
  queriedSettings <- withSettingsOverrides newOpts $ getSystemSettings brig
  liftIO $
    systemSettingsSetRestrictUserCreation queriedSettings @?= systemSettingsSetRestrictUserCreation testSettings

getSystemSettings ::
  (HasCallStack, MonadIO m, MonadHttp m, MonadCatch m, MonadThrow m) =>
  Brig ->
  m SystemSettings
getSystemSettings brig =
  responseJsonError
    =<< get (brig . path "/system/settings")
      <!! statusCode
        Bilge.Assert.=== const 200
