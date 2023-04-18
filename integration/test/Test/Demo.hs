{-# LANGUAGE FlexibleContexts #-}

-- | This module is meant to show how the integration can be used
module Test.Demo where

import API
import App
import Config
import qualified Data.Aeson as A
import Data.Default
import Imports
import SetupHelpers
import Text.RawString.QQ

-- | Cannot delete a legalhold client
testCantDeleteLHClient :: HasCallStack => App ()
testCantDeleteLHClient = do
  user <- randomUser def
  lhClientId <- bindResponse (addClient user def {ctype = "legalhold", internal = True}) $ \resp -> do
    resp.status @?= 201
    resp.json %. "id" & asString

  bindResponse (deleteClient user Nothing lhClientId) $ \resp -> do
    resp.status @?= 400

testDeleteUnknownClient :: HasCallStack => App ()
testDeleteUnknownClient = do
  user <- randomUser def
  let fakeClientId :: String = "deadbeefdeadbeef"
  bindResponse (deleteClient user Nothing fakeClientId) $ \resp -> do
    resp.status @?= 404
    resp.json %. "label" @%?= ("client-not-found" :: String)

testModifiedBrig :: HasCallStack => App ()
testModifiedBrig = do
  bindResponse getAPIVersion $ \resp ->
    (resp.json %. "domain") @%?= ("example.com" :: String)

  withModifiedService
    Brig
    ("optSettings.setFederationDomain" %.= ("overridden.example.com" :: String))
    $ bindResponse getAPIVersion
    $ ( \resp ->
          (resp.json %. "domain") @%?= ("overridden.example.com" :: String)
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
        bindResponse (getTeamFeatureInternal "searchVisibility" tid) $ \res -> do
          res.status @?= 200
          res.json %. "status"

  do
    status <- getFeatureStatus
    status @%?= ("disabled" :: String)

  withModifiedService
    Galley
    ("settings.featureFlags.teamSearchVisibility" %.= ("enabled-by-default" :: String))
    $ do
      status <- getFeatureStatus
      status @%?= ("enabled" :: String)

json :: HasCallStack => LByteString -> A.Value
json = fromJust . A.decode

testJSONUpdate :: HasCallStack => App ()
testJSONUpdate = do
  let before =
        json
          [r|
        {
          "foo" : {
             "bar": 2
          }
       }
  |]

  let expected =
        json
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
        json
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
        json
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
