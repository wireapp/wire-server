module Test.LoggingSpec (spec) where

import Imports
import Control.Lens
import Data.String.Conversions (cs)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Application)
import Spar.App
import Spar.Run (mkApp)
import System.IO.Silently (capture)
import System.Logger as Log
import Util

import qualified Network.Wai.Utilities.Server as Wai
import qualified Network.Wai.Test as HW
import qualified Test.Hspec.Wai as HW
import qualified Test.Hspec.Wai.Internal as HW


spec :: SpecWith TestEnv
spec = describe "logging" $ do
  it "does not log newlines (see haddocks of simpleSettings)" $ do
    logger <- asks (^. teSparEnv . to sparCtxLogger)
    liftIO $ do
      (out, _) <- capture $ do
        Log.fatal logger $ Log.msg ("hrgh\n\nwoaa" :: Text)
        Log.flush logger
      out `shouldContain` "hrgh  woaa"
      out `shouldNotContain` "hrgh\n\nwoaa"

  let withApp :: (Application -> Env -> Expectation) -> TestSpar ()
      withApp testcase = do
        (app, env) <- liftIO . mkApp =<< view teOpts
        liftIO $ testcase app env

      -- Test the 'WU.catchErrorsResponse' 'Middleware'.
      testCatchErrorsResponse :: HW.WaiSession HW.SResponse -> Int -> TestSpar ()
      testCatchErrorsResponse badReq expectedStatus = withApp $ \app env -> do
        (out, resp) <- capture $ do
          resp <- HW.withApplication app $ badReq
          Log.flush (sparCtxLogger env)
          pure resp
        statusCode (HW.simpleStatus resp) `shouldBe` expectedStatus
        out `shouldNotContain` cs Wai.catchErrorsResponseMsg

  it "Errors are thrown as IO Error, not handled by servant (1)" $
    testCatchErrorsResponse (HW.post "/sso/finalize-login" "") 400

  it "Errors are thrown as IO Error, not handled by servant (2)" $
    testCatchErrorsResponse (HW.get "/no/such/path") 404

  context "loglevel == debug" $ do
    it "400 on finalize-login causes log of entire request" . withApp $ \app env -> do
      let badbody = "@@badxml"
      (out, resp) <- capture $ do
        resp <- HW.withApplication app $ HW.post "/sso/finalize-login" badbody
        Log.flush (sparCtxLogger env)
        pure resp
      statusCode (HW.simpleStatus resp) `shouldBe` 400
      out `shouldContain` cs badbody
