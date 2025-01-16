module Test.Provider where

import API.Brig
import qualified API.Cargohold as Cargohold
import API.Common
import qualified API.Nginz as Nginz
import Data.String.Conversions (cs)
import SetupHelpers
import Testlib.Prelude

testProviderUploadAsset :: (HasCallStack) => App ()
testProviderUploadAsset = do
  alice <- randomUser OwnDomain def
  provider <- setupProvider alice def
  providerEmail <- provider %. "email" & asString
  pid <- provider %. "id" & asString
  -- test cargohold API
  bindResponse (Cargohold.uploadProviderAsset OwnDomain pid "profile pic") $ \resp -> do
    resp.status `shouldMatchInt` 201
  pw <- provider %. "password" & asString
  cookie <- loginProvider OwnDomain providerEmail pw
  -- test Nginz API
  bindResponse (Nginz.uploadProviderAsset OwnDomain (cs cookie) "another profile pic") $ \resp -> do
    resp.status `shouldMatchInt` 201

testProviderSearchWhitelist :: (HasCallStack) => App ()
testProviderSearchWhitelist =
  withModifiedBackend
    def
      { brigCfg =
          -- Disable password hashing rate limiting, so we can create enable services quickly
          setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
      }
    $ \domain -> do
      (owner, tid, [user]) <- createTeam domain 2
      provider <- setupProvider owner def {newProviderPassword = Just defPassword}
      pid <- asString $ provider %. "id"
      namePrefix <- randomString 10

      services <-
        forM (taggedServiceNames namePrefix) $ \(name, tags) -> do
          newService domain pid def {newServiceName = name, newServiceTags = tags}

      forM_ services $ \service -> do
        serviceId <- asString $ service %. "id"
        updateServiceConn pid serviceId (object ["password" .= defPassword])
          >>= assertSuccess
        postServiceWhitelist owner tid (object ["id" .= serviceId, "provider" .= pid, "whitelisted" .= True])
          >>= assertSuccess
      listTeamServiceProfilesByPrefix user tid Nothing False 20 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "services" `shouldMatchSet` services
      undefined

-- | A list of 20 services, all having names that begin with the given prefix.
--
-- NB: in some of the tests above, we depend on the fact that there are
-- exactly 20 services here.
taggedServiceNames :: String -> [(String, [String])]
taggedServiceNames prefix =
  [ (prefixed "Alpha", ["social", "quiz", "business"]),
    (prefixed "Beta", ["social", "music", "lifestyle"]),
    (prefixed "bjorn", ["social", "quiz", "travel"]),
    (prefixed "Bjørn", ["social", "music", "lifestyle"]),
    (prefixed "CHRISTMAS", ["social", "quiz", "weather"]),
    (prefixed "Delta", ["social", "music", "lifestyle"]),
    (prefixed "Epsilon", ["social", "quiz", "business"]),
    (prefixed "Freer", ["social", "music", "lifestyle"]),
    (prefixed "Gamma", ["social", "quiz", "weather"]),
    (prefixed "Gramma", ["social", "music", "lifestyle"]),
    (prefixed "Hera", ["social", "quiz", "travel"]),
    (prefixed "Io", ["social", "music", "lifestyle"]),
    (prefixed "Jojo", ["social", "quiz", "weather"]),
    (prefixed "Kuba", ["social", "music", "lifestyle"]),
    (prefixed "Lawn", ["social", "quiz", "travel"]),
    (prefixed "Mango", ["social", "music", "lifestyle"]),
    (prefixed "North", ["social", "quiz", "weather"]),
    (prefixed "Yak", ["social", "music", "lifestyle"]),
    (prefixed "Zeta", ["social", "quiz", "travel"]),
    (prefixed "Zulu", ["social", "music", "lifestyle"])
  ]
  where
    prefixed n = (prefix <> "|" <> n)
