module Test.LoggingSpec (spec) where

import Imports
import Control.Lens
import Spar.App
import System.Logger as Log
import System.IO.Silently (capture)
import Util

import qualified System.Logger as Log


spec :: HasCallStack => SpecWith TestEnv
spec = describe "logging" $ do
  it "does not log newlines" $ do
    logger <- asks (^. teSparEnv . to sparCtxLogger)
    liftIO $ do
      (out, _) <- capture $ Log.fatal logger $ Log.msg ("hrgh\n\nwoaa" :: Text)
      out `shouldContain` "hrgh  woaa"
      out `shouldNotContain` "hrgh\n\nwoaa"

  it "`format = Nothing`, `format = Just iso8601UTC` are equivalent" $ do
    let check :: HasCallStack => Bool -> (Settings -> Settings) -> IO String
        check netstr fmt = do
          logger <- Log.new $ Log.simpleDefSettings Log.Debug netstr & fmt
          (out, _) <- capture . Log.fatal logger $ Log.msg ("blä" :: Text)
          pure out

    liftIO $ do
      out1 <- check True (setFormat Nothing)
      out2 <- check True (setFormat (Just Log.iso8601UTC))
      out1 `shouldBe` out2

    liftIO $ do
      out1 <- check False (setFormat Nothing)
      out2 <- check False (setFormat (Just Log.iso8601UTC))
      out1 `shouldBe` out2
