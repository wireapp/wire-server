module Test.MLS.Clients where

import qualified API.BrigInternal as I
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testGetMLSClients :: (HasCallStack) => App ()
testGetMLSClients = do
  alice <- randomUser OwnDomain def
  alice1 <- createWireClient alice def

  bindResponse (I.getMLSClients alice def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs <- resp.json & asList
    c <- assertOne cs
    c
      `shouldMatch` object
        [ "has_key_packages" .= False,
          "id" .= alice1.client,
          "mls_signature_key" .= object []
        ]

  keys <- initMLSClient def alice1

  bindResponse (I.getMLSClients alice def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs <- resp.json & asList
    c <- assertOne cs
    c
      `shouldMatch` object
        [ "has_key_packages" .= False,
          "id" .= alice1.client,
          "mls_signature_key" .= keys
        ]

  void $ uploadNewKeyPackage def alice1

  bindResponse (I.getMLSClients alice def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs <- resp.json & asList
    c <- assertOne cs
    c
      `shouldMatch` object
        [ "has_key_packages" .= True,
          "id" .= alice1.client,
          "mls_signature_key" .= keys
        ]
