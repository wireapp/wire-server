module Test.Migration.ConversationCodes where

import API.Galley
import Control.Applicative
import Control.Monad.Codensity
import Control.Monad.Reader
import SetupHelpers
import Test.Migration.Util (waitForMigration)
import Testlib.Prelude
import Testlib.ResourcePool

testConversationCodesMigration :: (HasCallStack) => TaggedBool "has-password" -> App ()
testConversationCodesMigration (TaggedBool hasPassword) = do
  resourcePool <- asks (.resourcePool)
  let pw = if hasPassword then Just "funky password" else Nothing

  -- TODO:
  --  - test delete in every phase
  --  - test expiry
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    ( admin,
      code1,
      [conv1, conv2, conv3, conv4, conv5],
      m1 : m2 : m3 : m4 : _
      ) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (admin, _, members) <- createTeam domain 6
      convs@(conv : _) <- replicateM 5 $ postConversation admin (allowGuests defProteus) >>= getJSON 201
      code <- genCode admin conv pw
      pure (admin, code, convs, members)

    code2 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      code2 <- genCode admin conv2 pw
      check admin m1 conv1 code1 pw
      check admin m1 conv2 code2 pw
      pure code2

    code3 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      code3 <- genCode admin conv3 pw
      check admin m2 conv1 code1 pw
      check admin m2 conv2 code2 pw
      check admin m2 conv3 code3 pw
      waitForMigration domain counterName
      pure code3

    code4 <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      code4 <- genCode admin conv4 pw
      check admin m3 conv1 code1 pw
      check admin m3 conv2 code2 pw
      check admin m3 conv3 code3 pw
      check admin m3 conv4 code4 pw
      pure code4

    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      code5 <- genCode admin conv5 pw
      check admin m4 conv1 code1 pw
      check admin m4 conv2 code2 pw
      check admin m4 conv3 code3 pw
      check admin m4 conv4 code4 pw
      check admin m4 conv5 code5 pw
  where
    check admin user conv code pw = do
      joinWithCode user conv code
      getCode admin conv pw `shouldMatch` code

genCode :: (HasCallStack, MakesValue user, MakesValue conv) => user -> conv -> Maybe String -> App (String, String)
genCode user conv pw =
  bindResponse (postConversationCode user conv pw Nothing) $ \resp -> do
    res <- getJSON 201 resp
    k <- res %. "data.key" & asString
    v <- res %. "data.code" & asString
    pure (k, v)

getCode :: (HasCallStack, MakesValue user, MakesValue conv) => user -> conv -> Maybe String -> App (String, String)
getCode user conv pw =
  bindResponse (getConversationCode user conv pw) $ \resp -> do
    res <- getJSON 200 resp
    k <- res %. "key" & asString
    v <- res %. "code" & asString
    pure (k, v)

joinWithCode :: (HasCallStack, MakesValue user) => user -> Value -> (String, String) -> App ()
joinWithCode user conv (k, v) =
  bindResponse (getJoinCodeConv user k v) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" `shouldMatch` (objQidObject conv & objId)

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.conversationCodes" db,
      backgroundWorkerCfg = setField "migrateConversationCodes" runMigration
    }

counterName :: String
counterName = "^wire_conv_codes_migration_finished"
