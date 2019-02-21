module Test.LoggingSpec (spec) where

import Imports
import Control.Lens
import Data.String.Conversions (ST)
import Spar.App
import System.Logger as Log
import System.IO.Silently (capture)
import Util


spec :: HasCallStack => SpecWith TestEnv
spec = describe "logging" $ do
  it "does not log newlines" $ do
    logger <- asks (^. teSparEnv . to sparCtxLogger)
    liftIO $ do
      (out, _) <- capture $ Log.fatal logger $ Log.msg ("hrgh\n\nwoaa" :: ST)
      out `shouldContain` "hrgh  woaa"
      out `shouldNotContain` "hrgh\n\nwoaa"
