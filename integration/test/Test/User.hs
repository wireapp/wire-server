{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.User where

import API.Brig
import API.BrigInternal
import API.GalleyInternal
import API.Spar
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import SetupHelpers
import Testlib.Prelude
import UnliftIO (pooledForConcurrentlyN_)

testCreateManyUsers :: (HasCallStack) => App ()
testCreateManyUsers =
  pooledForConcurrentlyN_ 64 (replicate 1_024 def) \u -> do
    putStrLn "created 64 users"
    randomUser OwnDomain u

testSupportedProtocols :: (HasCallStack) => Domain -> App ()
testSupportedProtocols bobDomain = do
  alice <- randomUser OwnDomain def
  alice %. "supported_protocols" `shouldMatchSet` ["proteus"]

  bob <- randomUser bobDomain def

  do
    -- bob sees default supported protocols for alice
    u <- getUser bob alice >>= getJSON 200
    u %. "supported_protocols" `shouldMatch` ["proteus"]

    p <- getUserSupportedProtocols bob alice >>= getJSON 200
    p `shouldMatch` ["proteus"]

  -- alice updates her supported protocols
  bindResponse (putUserSupportedProtocols alice ["proteus", "mls"]) $ \resp ->
    resp.status `shouldMatchInt` 200

  do
    -- bob sees the updated list
    u <- getUser bob alice >>= getJSON 200
    u %. "supported_protocols" `shouldMatchSet` ["proteus", "mls"]

    p <- getUserSupportedProtocols bob alice >>= getJSON 200
    p `shouldMatch` ["proteus", "mls"]

  -- invalid protocol name in update
  bindResponse (putUserSupportedProtocols alice ["proteus", "mls", "mixed"]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

testCreateUserSupportedProtocols :: (HasCallStack) => App ()
testCreateUserSupportedProtocols = do
  alice <- randomUser OwnDomain def {supportedProtocols = Just ["proteus", "mls"]}
  bindResponse (getUserSupportedProtocols alice alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` ["proteus", "mls"]

  bindResponse (createUser OwnDomain def {supportedProtocols = Just ["proteus", "mixed"]}) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

-- | For now this only tests attempts to update /self/handle in E2EId-enabled teams.  More
-- tests can be found under `/services/brig/test/integration` (and should be moved here).
testUpdateHandle :: (HasCallStack) => App ()
testUpdateHandle = do
  -- create team with one member, without scim, but with `mlsE2EId` enabled.
  (owner, team, [mem1]) <- createTeam OwnDomain 2
  mem1id <- asString $ mem1 %. "id"

  let featureName = "mlsE2EId"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< setTeamFeatureStatus owner team featureName "enabled"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

  -- all as expected here.  (see the second time we check this at the end of the test for an
  -- explanation why we care.)
  bindResponse (getSelf mem1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "managed_by" `shouldMatch` "wire"
  bindResponse (getUsersId owner [mem1id]) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mb <- (assertOne =<< asList resp.json) %. "managed_by"
    mb `shouldMatch` "wire"

  -- mem1 attempts to update handle for the first time => success
  --
  -- this is desired, because without SCIM users need to pick their own handles initially.
  -- moreover it is fine, because if `handle == NULL`, no mls E2Eid client certs can be
  -- created.
  handle <- UUID.toString <$> liftIO UUID.nextRandom
  bindResponse (putHandle mem1 handle) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (putHandle mem1 handle) $ \resp -> do
    -- idempotency
    resp.status `shouldMatchInt` 200

  -- mem1 attempts to update handle again => failure
  handle2 <- UUID.toString <$> liftIO UUID.nextRandom
  bindResponse (putHandle mem1 handle2) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "managed-by-scim"

  -- now self thinks it is managed by "scim", so clients can block change attempts to handle,
  -- display name without adding E2EId-specific logic.  this is just a hack, though: `GET
  -- /self` is the only place where this is happening, other end-points still report the truth
  -- that is still stored correctly in the DB.
  --
  -- details: https://wearezeta.atlassian.net/browse/WPB-6189.
  -- FUTUREWORK: figure out a better way for clients to detect E2EId (V6?)
  bindResponse (getSelf mem1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "managed_by" `shouldMatch` "scim"
  bindResponse (getUsersId owner [mem1id]) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mb <- (assertOne =<< asList resp.json) %. "managed_by"
    mb `shouldMatch` "wire"
  bindResponse (getScimTokens owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "tokens" `shouldMatch` ([] @String)

-- | For now this only tests attempts to update one's own display name, email address, or
-- language in E2EId-enabled teams (ie., everything except handle).  More tests can be found
-- under `/services/brig/test/integration` (and should be moved here).
testUpdateSelf :: (HasCallStack) => Tagged "mode" TestUpdateSelfMode -> App ()
testUpdateSelf (MkTagged mode) = do
  -- create team with one member, without scim, but with `mlsE2EId` enabled.
  (owner, team, [mem1]) <- createTeam OwnDomain 2

  let featureName = "mlsE2EId"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "disabled"
  assertSuccess =<< setTeamFeatureStatus owner team featureName "enabled"
  bindResponse (getTeamFeature owner team featureName) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` "enabled"

  case mode of
    TestUpdateDisplayName -> do
      -- blocked unconditionally
      someDisplayName <- UUID.toString <$> liftIO UUID.nextRandom
      before <- getSelf mem1
      bindResponse (putSelf mem1 def {name = Just someDisplayName}) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "managed-by-scim"
      after <- getSelf mem1
      void $ (before.json %. "name") `shouldMatch` (after.json %. "name")
    TestUpdateEmailAddress -> do
      -- allowed unconditionally *for owner* (this is a bit off-topic: team members can't
      -- change their email addresses themselves under any conditions)
      someEmail <- (<> "@example.com") . UUID.toString <$> liftIO UUID.nextRandom
      bindResponse (putSelfEmail owner someEmail) $ \resp -> do
        resp.status `shouldMatchInt` 200
    TestUpdateLocale -> do
      -- scim maps "User.preferredLanguage" to brig's locale field.  allowed unconditionally.
      -- we try two languages to make sure it doesn't work because it's already the active
      -- locale.
      forM_ ["uk", "he"] $ \someLocale ->
        bindResponse (putSelfLocale mem1 someLocale) $ \resp -> do
          resp.status `shouldMatchInt` 200

data TestUpdateSelfMode
  = TestUpdateDisplayName
  | TestUpdateEmailAddress
  | TestUpdateLocale
  deriving (Eq, Show, Generic)

testActivateAccountWithPhoneV5 :: (HasCallStack) => App ()
testActivateAccountWithPhoneV5 = do
  let dom = OwnDomain
  let phone = "+4912345678"
  let reqBody = Aeson.object ["phone" .= phone]
  activateUserV5 dom reqBody `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "invalid-phone"
