module FanInPerf.ServiceSpec (spec) where

import Control.Exception (AsyncException (ThreadKilled), ErrorCall (..), throwIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Id
import Data.UUID.Types qualified as UUID
import FanInPerf.Service
import Imports
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAllow)
import Network.Wai qualified as Wai
import Network.Wai.Test
import Test.Hspec

spec :: Spec
spec = describe "experimental Gundeck store API" $ do
  let fallback _ respond = respond $ Wai.responseLBS status404 [] ""
      app execute = serviceMiddleware execute fallback
      requestFor endpoint body =
        SRequest
          ((setPath Wai.defaultRequest ("/i/fan-in-perf/" <> endpoint)) {Wai.requestMethod = methodPost})
          body
      forbidden _ = expectationFailure "store must not be called" $> Right Null
      user = Id UUID.nil
      client = ClientId 1
      cases =
        [ ("publish", "{\"payload\":{}}", Publish mempty),
          ("fetch", "{\"cursor\":\"00000000-0000-0000-0000-000000000000\",\"limit\":100}", Fetch UUID.nil 100),
          ("initialize", "{\"user\":\"00000000-0000-0000-0000-000000000000\",\"client\":\"1\"}", Initialize user client),
          ("acknowledge", "{\"user\":\"00000000-0000-0000-0000-000000000000\",\"client\":\"1\",\"notification\":\"00000000-0000-0000-0000-000000000000\"}", Acknowledge user client UUID.nil)
        ]
  for_ cases $ \(endpoint, body, expected) -> do
    it ("dispatches " <> show endpoint <> " once") $ do
      calls <- newIORef (0 :: Int)
      let execute command = do
            command `shouldBe` expected
            modifyIORef' calls (+ 1)
            pure $ Right Null
      result <- runSession (srequest $ requestFor endpoint body) (app execute)
      simpleStatus result `shouldBe` status200
      readIORef calls `shouldReturn` 1
    it ("rejects a missing body for " <> show endpoint) $ do
      result <- runSession (srequest $ requestFor endpoint "{}") (app forbidden)
      simpleStatus result `shouldBe` status400
    it ("rejects GET for " <> show endpoint) $ do
      result <- runSession (request $ setPath Wai.defaultRequest ("/i/fan-in-perf/" <> endpoint)) (app forbidden)
      simpleStatus result `shouldBe` status405
      lookup hAllow (simpleHeaders result) `shouldBe` Just "POST"
  it "bounds the request body" $ do
    result <- runSession (srequest $ requestFor "publish" (BL.replicate 65537 32)) (app forbidden)
    simpleStatus result `shouldBe` status413
  it "rejects unbounded fetch requests" $ do
    result <- runSession (srequest $ requestFor "fetch" "{\"cursor\":\"00000000-0000-0000-0000-000000000000\",\"limit\":1001}") (app forbidden)
    simpleStatus result `shouldBe` status400
  it "rejects a body intended for another endpoint" $ do
    result <- runSession (srequest $ requestFor "publish" "{\"operation\":\"fetch\",\"cursor\":\"00000000-0000-0000-0000-000000000000\",\"limit\":100}") (app forbidden)
    simpleStatus result `shouldBe` status400
  it "returns a generic failure when storage fails" $ do
    result <- runSession (srequest $ requestFor "publish" "{\"payload\":{}}") (app (const $ pure $ Left ()))
    simpleStatus result `shouldBe` status503
    simpleBody result `shouldBe` "null"
  for_ ["/", "/i/fan-in-perf", "/i/fan-in-perf/unknown"] $ \path ->
    it ("leaves unmatched path " <> show path <> " to Gundeck") $ do
      result <- runSession (request $ setPath Wai.defaultRequest path) (app forbidden)
      simpleStatus result `shouldBe` status404

  describe "persistence and wakeup ordering" $ do
    it "persists before sending one wakeup" $ do
      steps <- newIORef ([] :: [Text])
      let store _ = modifyIORef' steps (<> ["persist"]) $> Right Null
          wakeup = modifyIORef' steps (<> ["wakeup"])
      executeWithWakeup store wakeup (Publish mempty) `shouldReturn` Right Null
      readIORef steps `shouldReturn` ["persist", "wakeup"]
    it "does not send a wakeup when persistence fails" $ do
      executeWithWakeup (const $ pure $ Left ()) (expectationFailure "unexpected wakeup") (Publish mempty) `shouldReturn` Left ()
    it "returns failure without retrying persistence if the wakeup fails" $ do
      writes <- newIORef (0 :: Int)
      let store _ = modifyIORef' writes (+ 1) $> Right Null
      executeWithWakeup store (throwIO $ ErrorCall "synthetic broker failure") (Publish mempty) `shouldReturn` Left ()
      readIORef writes `shouldReturn` 1
    it "does not swallow cancellation while sending a wakeup" $
      executeWithWakeup (const $ pure $ Right Null) (throwIO ThreadKilled) (Publish mempty) `shouldThrow` (== ThreadKilled)
    for_ [Fetch UUID.nil 100, Initialize user client, Acknowledge user client UUID.nil] $ \command ->
      it ("does not signal for " <> show command) $
        executeWithWakeup (const $ pure $ Right Null) (expectationFailure "unexpected wakeup") command `shouldReturn` Right Null
