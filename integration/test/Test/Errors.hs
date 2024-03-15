{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Errors where

import API.Brig
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import SetupHelpers
import System.IO.Temp
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
    mockConfig <- do
      mBase <- asks (.servicesCwdBase)
      pure $ case mBase of
        Just _ ->
          -- when running locally, spawn a fake ingress returning an error
          def
            { port = Just (fromIntegral res.berNginzSslPort),
              tls = True
            }
        Nothing -> do
          -- on CI, the real federation ingress is available, so we spawn its federator upstream instead
          def
            { port = Just (fromIntegral res.berFederatorExternal),
              tls = False
            }
    void $
      startMockServer mockConfig $
        codensityApp $
          \_req -> pure $ Wai.responseLBS HTTP.status400 mempty $ Aeson.encode innerError

    -- get remote user
    lift $ do
      user <- randomUser OwnDomain def
      targetId <- randomId
      let target = object ["id" .= targetId, "domain" .= res.berDomain]
      bindResponse (getUser user target) $ \resp -> do
        resp.status `shouldMatchInt` 533
        resp.json %. "inner" `shouldMatch` innerError

testNginzLogs :: HasCallStack => App ()
testNginzLogs = do
  withSystemTempDirectory "nginz" $ \tmp -> do
    let errorLog = tmp <> "nginz.log"
        overrides =
          def
            { nginzCfg = setField "error_log" errorLog
            }
    withModifiedBackend overrides $ \domain -> do
      alice <- randomUser domain def
      req <- rawBaseRequest alice Nginz Versioned (joinHttpPath ["foo"])
      void $
        submit
          "GET"
          (addQueryParams [("access_token", "s3cret")] req)
          >>= getBody 401

    logContents <- readFile errorLog
    for_ (lines logContents) $ \line -> do
      assertBool
        ("found secret in log:\n" <> line)
        (not (isInfixOf "access_token=s3cret" line))
