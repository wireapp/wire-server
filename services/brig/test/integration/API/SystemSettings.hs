module API.SystemSettings (tests) where

import Bilge
import qualified Brig.Options as Opts
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Control.Lens.Operators
import Wire.API.Routes.Public.Brig
import Control.Monad.Catch
import Bilge.Assert

tests :: Opts.Opts -> Manager -> Brig -> IO TestTree
tests opts m brig = pure $ do
  testGroup
    "settings"
    [ test m "GET /system/settings" $ testGetSettings opts brig
    ]

testGetSettings :: Opts.Opts -> Brig -> Http ()
testGetSettings opts brig = forM_ [False .. True] $ \v -> do
  let newOpts = opts & (Opts.optionSettings . Opts.restrictUserCreation) ?~ v
  settings <- withSettingsOverrides newOpts $ getSystemSettings brig
  liftIO $ systemSettingsSetRestrictUserCreation settings @?= Just v

getSystemSettings :: (HasCallStack, MonadIO m, MonadHttp m, MonadCatch m, MonadThrow m) =>
  Brig -> m SystemSettings
getSystemSettings brig =
  responseJsonError =<< get (brig . path "/system/settings") <!!
    statusCode === const 200
