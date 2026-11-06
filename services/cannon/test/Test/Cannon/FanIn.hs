module Test.Cannon.FanIn (tests) where

import Cannon.FanIn qualified as FanIn
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Proxy
import FanInPerf.Service (wakeupPayload)
import FanInPerf.Wakeup
import Imports
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Test
import Servant
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "experimental Servant routes"
    [ testCase "disabled wakeup returns 404" $ do
        response <- runSession (srequest $ postWakeup $ encode wakeupPayload) (app Nothing)
        simpleStatus response @?= status404,
      testCase "enabled wakeup advances the hub" $ do
        hub <- newWakeupHub
        response <- runSession (srequest $ postWakeup $ encode wakeupPayload) (enabled hub)
        simpleStatus response @?= status204
        atomically (currentGeneration hub) >>= (@?= 1),
      testCase "malformed and oversized wakeups do not signal" $ do
        hub <- newWakeupHub
        for_ [("{}", status400), ("invalid", status400), (BL.replicate 1025 32, status413)] $ \(body, expected) -> do
          response <- runSession (srequest $ postWakeup body) (enabled hub)
          simpleStatus response @?= expected
        atomically (currentGeneration hub) >>= (@?= 0),
      testCase "wakeup requires POST" $ do
        response <- runSession (request $ setPath Wai.defaultRequest "/i/fan-in-perf/wakeup") (app Nothing)
        simpleStatus response @?= status405,
      testCase "WebSocket identity headers are required" $ do
        response <- runSession (request $ setPath Wai.defaultRequest "/i/fan-in-perf/events") (app Nothing)
        simpleStatus response @?= status400,
      testCase "valid identity requires a WebSocket upgrade" $ do
        let req =
              (setPath Wai.defaultRequest "/i/fan-in-perf/events")
                { Wai.requestHeaders = [("Z-User", "00000000-0000-0000-0000-000000000001"), ("Z-Client", "1"), ("Z-Connection", "test-connection")]
                }
        response <- runSession (request req) (app Nothing)
        statusCode (simpleStatus response) @?= 426,
      testCase "unmatched paths remain 404" $ do
        response <- runSession (request $ setPath Wai.defaultRequest "/i/fan-in-perf/unknown") (app Nothing)
        simpleStatus response @?= status404
    ]
  where
    -- These HTTP routing tests must never access the DB or WebSocket environment.
    app runtime = serve (Proxy @FanIn.API) (FanIn.server runtime (error "unexpected WS environment access"))
    enabled hub = app $ Just $ FanIn.Runtime (error "unexpected store access") hub
    postWakeup body =
      SRequest
        ( (setPath Wai.defaultRequest "/i/fan-in-perf/wakeup")
            { Wai.requestMethod = methodPost,
              Wai.requestHeaders = [(hContentType, "application/json")]
            }
        )
        body
