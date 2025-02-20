module Test.Provider where

import API.Brig
import API.BrigInternal
import qualified API.Cargohold as Cargohold
import API.Common
import qualified API.Nginz as Nginz
import Data.String.Conversions (cs)
import SetupHelpers
import Testlib.Prelude

testProviderUploadAsset :: (HasCallStack) => App ()
testProviderUploadAsset = do
  alice <- randomUser OwnDomain def
  provider <- setupProvider alice def {newProviderPassword = Just defPassword}
  providerEmail <- provider %. "email" & asString
  pid <- provider %. "id" & asString
  -- test cargohold API
  bindResponse (Cargohold.uploadProviderAsset OwnDomain pid "profile pic") $ \resp -> do
    resp.status `shouldMatchInt` 201
  cookie <-
    loginProvider OwnDomain providerEmail defPassword `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      let hs = headers resp
          setCookieHeader = fromString "Set-Cookie"
      pure . fromJust . foldMap (\(k, v) -> guard (k == setCookieHeader) $> v) $ hs

  -- test Nginz API
  bindResponse (Nginz.uploadProviderAsset OwnDomain (cs cookie) "another profile pic") $ \resp -> do
    resp.status `shouldMatchInt` 201

testProviderPasswordReset :: (HasCallStack) => App ()
testProviderPasswordReset = do
  withModifiedBackend
    def
      { brigCfg =
          -- Disable password hashing rate limiting, so we can create enable services quickly
          setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
      }
    $ \domain -> do
      provider <- setupProvider domain def {newProviderPassword = Just defPassword}
      email <- asString $ provider %. "email"
      requestProviderPasswordResetCode domain email >>= assertSuccess
      resetCode <- getProviderPasswordResetCodeInternal domain email >>= getJSON 200

      completeProviderPasswordReset domain resetCode defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "password-must-differ"

      completeProviderPasswordReset domain resetCode "shiny-new-password" >>= assertSuccess
      loginProvider domain email "shiny-new-password" >>= assertSuccess
      loginProvider domain email defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "invalid-credentials"

testProviderPasswordResetAfterEmailUpdate :: (HasCallStack) => App ()
testProviderPasswordResetAfterEmailUpdate = do
  withModifiedBackend
    def
      { brigCfg =
          -- Disable password hashing rate limiting, so we can create enable services quickly
          setField @_ @Int "optSettings.setPasswordHashingRateLimit.userLimit.inverseRate" 0
      }
    $ \domain -> do
      provider <- setupProvider domain def {newProviderPassword = Just defPassword}
      origEmail <- asString $ provider %. "email"
      pid <- asString $ provider %. "id"
      newEmail <- randomEmail
      requestProviderPasswordResetCode domain origEmail >>= assertSuccess
      requestProviderEmailUpdateCode domain pid newEmail >>= assertSuccess
      passwordResetCode <- getProviderPasswordResetCodeInternal domain origEmail >>= getJSON 200
      emailUpdateKeyCodePair <- getProviderActivationCodeInternal domain newEmail >>= getJSON 200
      emailUpdateKey <- asString $ emailUpdateKeyCodePair %. "key"
      emailUpdateCode <- asString $ emailUpdateKeyCodePair %. "code"

      activateProvider domain emailUpdateKey emailUpdateCode

      completeProviderPasswordReset domain passwordResetCode "shiny-new-password" `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "invalid-code"

      loginProvider domain origEmail defPassword `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "invalid-credentials"
      loginProvider domain newEmail defPassword >>= assertSuccess

      requestProviderPasswordResetCode domain newEmail >>= assertSuccess
      newPasswordResetCode <- getProviderPasswordResetCodeInternal domain newEmail >>= getJSON 200
      completeProviderPasswordReset domain newPasswordResetCode "shiny-new-password" >>= assertSuccess
      loginProvider domain newEmail "shiny-new-password" >>= assertSuccess

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
          createAndEnableService domain owner tid pid def {newServiceName = name, newServiceTags = tags}

      allServiceIds <- traverse (%. "id") services

      -- Searching with the common prefix shows all of them
      listTeamServiceProfilesByPrefix user tid (Just namePrefix) False 20 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        asListOf (%. "id") (resp.json %. "services") `shouldMatch` allServiceIds

      -- Searching without filtering returns all of them because all are enabled
      listTeamServiceProfilesByPrefix user tid (Just namePrefix) True 20 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        asListOf (%. "id") (resp.json %. "services") `shouldMatch` allServiceIds

      -- Search should yield services ordered by named
      zerosPrefixedService <- do
        serviceSuffix <- randomString 10
        createAndEnableService domain owner tid pid def {newServiceName = "0000000000|" <> serviceSuffix, newServiceTags = ["social"]}
      listTeamServiceProfilesByPrefix user tid Nothing True 20 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        firstServiceId <- (%. "id") . head =<< asList (resp.json %. "services")
        firstServiceId `shouldMatch` (zerosPrefixedService %. "id")

      -- Search by exact name yields only one service
      forM_ (take 3 services) $ \service -> do
        name <- asString $ service %. "name"
        listTeamServiceProfilesByPrefix user tid (Just name) False 20 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "has_more" `shouldMatch` False
          asListOf (%. "name") (resp.json %. "services") `shouldMatch` [name]

      -- Search by prefix, case insensitve, doesn't asciify special characters
      -- like name search
      forM_ [("Bjø", "Bjørn"), ("Bjo", "bjorn"), ("chris", "CHRISTMAS")] $ \(searchTerm, hardcodedName) ->
        listTeamServiceProfilesByPrefix user tid (Just $ namePrefix <> "|" <> searchTerm) False 20 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "has_more" `shouldMatch` False
          asListOf (%. "name") (resp.json %. "services") `shouldMatch` [(namePrefix <> "|" <> hardcodedName)]

      -- Search works even after changing name
      let alphaService = head services
          newAlphaName = namePrefix <> "|" <> "NotAlphaAnyMore"
      alphaServiceId <- alphaService %. "id"
      updateService domain pid alphaServiceId Nothing (Just newAlphaName)
        >>= assertSuccess
      listTeamServiceProfilesByPrefix user tid (Just newAlphaName) False 20 `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "services.0.id" `shouldMatch` alphaServiceId
        resp.json %. "services.0.name" `shouldMatch` newAlphaName

createAndEnableService ::
  (MakesValue domain, MakesValue user) =>
  domain ->
  user ->
  String ->
  String ->
  NewService ->
  App Value
createAndEnableService domain teamAdmin tid pid newSvc = do
  serviceId <- (newService domain pid newSvc) %. "id" & asString
  -- serviceId <- asString $ service %. "id"
  updateServiceConn domain pid serviceId (object ["password" .= defPassword, "enabled" .= True])
    >>= assertSuccess
  postServiceWhitelist teamAdmin tid (object ["id" .= serviceId, "provider" .= pid, "whitelisted" .= True])
    >>= assertSuccess
  getService domain pid serviceId >>= getJSON 200

-- | A list of 20 services ordered alphabetically, all having names that begin
-- with the given prefix.
--
-- NB: in some of the tests above, we depend on the fact that there are exactly
-- 20 services here and the fact that they are ordered alphabetically.
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
