{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Errors where

import API.Brig
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import SetupHelpers
import Testlib.Mock
import Testlib.Prelude
import Testlib.ResourcePool

testNestedError :: HasCallStack => App ()
testNestedError = do
  let innerError =
        object
          [ "code" .= (400 :: Int),
            "label" .= "example",
            "message" .= "Example remote federator failure"
          ]

  resourcePool <- asks resourcePool
  lowerCodensity $ do
    [res] <- acquireResources 1 resourcePool
    void
      $ startMockServer
        def
          { port = Just (fromIntegral res.berNginzSslPort),
            tls = True
          }
      $ codensityApp
      $ \_req -> do
        liftIO $ putStrLn "received request"
        pure $ Wai.responseLBS HTTP.status400 mempty $ Aeson.encode innerError

    -- get remote user
    lift $ do
      user <- randomUser OwnDomain def
      targetId <- randomId
      let target = object ["id" .= targetId, "domain" .= res.berDomain]
      bindResponse (getUser user target) $ \resp -> do
        resp.status `shouldMatchInt` 533
        resp.json %. "inner" `shouldMatch` innerError
