module Test.LoggingSpec (spec) where

import Imports
import Control.Lens
import Spar.App
import System.Logger as Log
import System.IO.Silently (capture)
import Util


spec :: HasCallStack => SpecWith TestEnv
spec = describe "logging" $ do
  it "does not log newlines (see haddocks of simpleDefSettings)" $ do
    logger <- asks (^. teSparEnv . to sparCtxLogger)
    liftIO $ do
      (out, _) <- capture $ do
        Log.fatal logger $ Log.msg ("hrgh\n\nwoaa" :: Text)
        Log.flush logger
      out `shouldContain` "hrgh  woaa"
      out `shouldNotContain` "hrgh\n\nwoaa"

  describe "`format = Nothing`, `format = Just iso8601UTC` are equivalent" $ do
    let check :: HasCallStack => Bool -> (Settings -> Settings) -> IO String
        check netstr fmt = do
          logger <- Log.new $ Log.simpleDefSettings Log.Debug netstr & fmt
          (out, _) <- capture $ do
            Log.fatal logger $ Log.msg ("blä" :: Text)
            Log.flush logger
          pure out

    it "netstr renderer" . liftIO $ do
      out1 <- check True (setFormat Nothing)
      out2 <- check True (setFormat (Just Log.iso8601UTC))
      out1 `shouldBe` out2

    it "default renderer" . liftIO $ do
      out1 <- check False (setFormat Nothing)
      out2 <- check False (setFormat (Just Log.iso8601UTC))
      out1 `shouldBe` out2
