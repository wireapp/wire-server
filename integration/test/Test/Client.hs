{-# LANGUAGE FlexibleContexts #-}

module Test.Client where

import API
import App
import Config
import Data.Default
import Imports
import SetupHelpers

-- | Cannot delete a legalhold client
--
-- More comments
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

testModifiedServices :: HasCallStack => App ()
testModifiedServices = do
  bindResponse getBrigAPIVersion $ \resp ->
    (resp.json %. "domain") @%?= ("example.com" :: String)

  withModifiedService
    Brig
    ("optSettings.setFederationDomain" %.= ("overridden.example.com" :: String))
    $ bindResponse getBrigAPIVersion
    $ ( \resp ->
          (resp.json %. "domain") @%?= ("overridden.example.com" :: String)
      )
