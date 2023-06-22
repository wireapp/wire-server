module Test.B2B where

import qualified API.Brig as Public
import Control.Lens
import Data.Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens
import SetupHelpers
import Testlib.Prelude

testConnectUsers :: App ()
testConnectUsers = do
  _alice <- randomUser OwnDomain def
  pure ()

testDeleteClient :: App ()
testDeleteClient = do
  alice <- randomUser OwnDomain def
  bob <- randomUser OtherDomain def
  void $
    Public.addClient alice def
      >>= getJSON 201

  Object bobClient <-
    Public.addClient bob def
      >>= getJSON 201

  assertOnDeleteClient alice bob bobClient

testDeleteTempClient :: App ()
testDeleteTempClient = do
  alice <- randomUser OwnDomain def
  bob <- randomUser OtherDomain def
  void $
    Public.addClient alice def {Public.ctype = "temporary"}
      >>= getJSON 201

  Object bobClient <-
    Public.addClient bob def {Public.ctype = "temporary"}
      >>= getJSON 201

  assertOnDeleteClient alice bob bobClient

assertOnDeleteClient ::
  ( MakesValue otherUser,
    MakesValue user,
    ToJSON v
  ) =>
  user ->
  otherUser ->
  KeyMap v ->
  App ()
assertOnDeleteClient alice bob bobClient = do
  let idKey = fromString "id"

  bindResponse (Public.getClientsQualified alice OtherDomain bob) $ \resp -> do
    let obj = resp.jsonBody ^? _Just . _Array . _head . _Object
    (KeyMap.lookup idKey <$> obj)
      `shouldMatch` (bobClient !? idKey)
    resp.status `shouldMatchInt` 200

  bindResponse (Public.deleteClient bob bobClient) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (Public.getClientsQualified alice OtherDomain bob) $ \resp -> do
    let obj = resp.jsonBody ^? _Just . _Array . _head . _Object
    (KeyMap.lookup idKey <$> obj)
      `shouldMatch` Null
    resp.status `shouldMatchInt` 200
