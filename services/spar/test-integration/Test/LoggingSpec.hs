module Test.LoggingSpec (spec) where

import Imports
import Control.Lens
import Spar.App
import System.Logger as Log
import System.IO.Silently (capture)
import Util


spec :: HasCallStack => SpecWith TestEnv
spec = describe "logging" $ do
  it "does not log newlines (see haddocks of simpleSettings)" $ do
    logger <- asks (^. teSparEnv . to sparCtxLogger)
    liftIO $ do
      (out, _) <- capture $ do
        Log.fatal logger $ Log.msg ("hrgh\n\nwoaa" :: Text)
        Log.flush logger
      out `shouldContain` "hrgh  woaa"
      out `shouldNotContain` "hrgh\n\nwoaa"
