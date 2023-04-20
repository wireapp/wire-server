{-# LANGUAGE FlexibleContexts #-}

-- | This module is meant to show how the integration can be used
module Test.Demo where

import qualified API
import Data.ByteString.Conversion
import Imports
import TestLib.Prelude

-- | Cannot delete a legalhold client
testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser def
  lhClientId <- bindResponse (API.addClient user def {API.ctype = "legalhold", API.internal = True}) $ \resp -> do
    resp.status @?= 201
    resp.json `getField` "id" & asString

  bindResponse (API.deleteClient user Nothing lhClientId) $ \resp -> do
    resp.status `shouldMatch` 400

testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser def
  let fakeClientId :: String = "deadbeefdeadbeef"
  bindResponse (API.deleteClient user Nothing fakeClientId) $ \resp -> do
    resp.status @?= 403
    resp.json `getField` "label" `shouldMatchJson` "client-not-found"

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  withModifiedService
    Brig
    (setField "optSettings.setFederationDomain" "overridden.example.com")
    $ bindResponse getAPIVersion
    $ ( \resp ->
          (resp.json %. "domain") `shouldMatchJson` "overridden.example.com"
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
          res.status @?= 200
          res.json %. "status"

  do
    status <- getFeatureStatus
    status `shouldMatchJson` "disabled"

  withModifiedService
    Galley
    (setField "settings.featureFlags.teamSearchVisibility" "enabled-by-default")
    $ do
      status <- getFeatureStatus
      status `shouldMatchJson` "enabled"

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

  (before & "foo.bar" %.= ("baaz" :: String)) @%?= expected

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

  (before & "foo.quux" %.= (3 :: Int)) @%?= expected2

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
    (\e -> take 23 e.msg @?= "Field \"quux\" is missing")
    (before & "foo.quux.zok" %.= ("eke" :: String))
