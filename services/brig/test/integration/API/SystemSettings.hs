module API.SystemSettings (tests) where

import Bilge
import qualified Brig.Options as Opts
import Imports
import Test.Tasty
-- import Test.Tasty.HUnit
import Util

tests :: Opts.Opts -> Manager -> Brig -> IO TestTree
tests _ m _ = pure $ do
  testGroup
    "settings"
    [ test m "GET /system/settings" testGetSettings
    ]

testGetSettings :: Http ()
testGetSettings = pure ()
