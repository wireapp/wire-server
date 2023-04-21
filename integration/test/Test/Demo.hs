{-# LANGUAGE FlexibleContexts #-}

-- | This module is meant to show how the integration can be used
module Test.Demo where

import qualified API
import Data.ByteString.Conversion
import Imports
import TestLib.Cannon (withWebSocket)
import TestLib.Prelude

-- | Cannot delete a legalhold client
testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser def
  lhClientId <- bindResponse (API.addClient user def {API.ctype = "legalhold", API.internal = True}) $ \resp -> do
    resp.status `shouldMatchPlain` 201
    resp.json %. "id" & asString

  bindResponse (API.deleteClient user Nothing lhClientId) $ \resp -> do
    resp.status `shouldMatchPlain` 400

testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser def
  let fakeClientId :: String = "deadbeefdeadbeef"
  bindResponse (API.deleteClient user Nothing fakeClientId) $ \resp -> do
    resp.status `shouldMatchPlain` 404
    resp.json %. "label" `shouldMatch` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedService
    Brig
    (setField "optSettings.setFederationDomain" "overridden.example.com")
    $ bindResponse getAPIVersion
    $ ( \resp ->
          (resp.json %. "domain") `shouldMatch` "overridden.example.com"
      )
  where
    getAPIVersion :: App Response
    getAPIVersion = do
      req <- baseRequest Brig Unversioned $ "/api-version"
      submit "GET" req

testModifiedGalley :: HasCallStack => App ()
testModifiedGalley = do
  (_user, tid) <- createTeam

  let getFeatureStatus = do
        bindResponse (API.getTeamFeatureInternal "searchVisibility" tid) $ \res -> do
          res.status `shouldMatchPlain` 200
          res.json %. "status"

  do
    status <- getFeatureStatus
    status `shouldMatch` "disabled"

  withModifiedService
    Galley
    (setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default")
    $ do
      status <- getFeatureStatus
      status `shouldMatch` "enabled"

jsonValue :: HasCallStack => String -> Value
jsonValue = fromJust . decode . toByteString

testJSONUpdate :: HasCallStack => App ()
testJSONUpdate = do
  let before =
        jsonValue
          [r|
        {
          "foo" : {
             "bar": 2
          }
       }
  |]

  let expected =
        jsonValue
          [r|
        {
          "foo" : {
             "bar": "baaz"
          }
       }
  |]

  (before & setField "foo.bar" "baaz") `shouldMatch` expected

  -- test case: when last field doesn't exist

  let expected2 =
        jsonValue
          [r|
        {
          "foo" : {
             "bar": 2,
             "quux": 3
          }
       }
  |]

  (before & setField "foo.quux" (3 :: Int)) `shouldMatch` expected2

testJSONUpdateFailure :: HasCallStack => App ()
testJSONUpdateFailure = do
  let before =
        jsonValue
          [r|
        {
          "foo" : {
             "bar": 2
          }
       }
  |]

  expectFailure
    (\e -> take 23 e.msg `shouldMatch` "Field \"quux\" is missing")
    (before & setField "foo.quux.zok" "eke")

testWebSockets :: HasCallStack => App ()
testWebSockets = do
  user <- randomUser def
  withWebSocket user $ \_ws -> do
    putStrLn "got websocket"
  pure ()
