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
  --  - test expiry
  runCodensity (acquireResources 1 resourcePool) $ \[backend] -> do
    let domain = backend.berDomain

    (admin, code1, codeA, convs, members) <- runCodensity (startDynamicBackend backend (conf "cassandra" False)) $ \_ -> do
      (admin, _, members) <- createTeam domain 6
      convs1@(conv1 : _) <- replicateM 5 $ postConversation admin (allowGuests defProteus) >>= getJSON 201
      convs2@(convA : _) <- replicateM 4 $ postConversation admin (allowGuests defProteus) >>= getJSON 201
      code1 <- genCode admin conv1 pw
      codeA <- genCode admin convA pw
      pure (admin, code1, codeA, convs1 <> convs2, members)

    [conv1, conv2, conv3, conv4, conv5, convA, convB, convC, convD] <- pure convs
    m1 : m2 : m3 : m4 : _ <- pure members

    (code2, codeB) <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      -- code generation works
      code2 <- genCode admin conv2 pw
      codeB <- genCode admin convB pw
      -- joining works
      checkJoinAndGet admin m1 conv1 code1 pw
      checkJoinAndGet admin m1 conv2 code2 pw
      -- deletion works
      checkDelete admin m1 convA codeA pw
      pure (code2, codeB)

    (code3, codeC) <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" True)) $ \_ -> do
      -- code generation works
      code3 <- genCode admin conv3 pw
      codeC <- genCode admin convC pw
      -- joining works
      checkJoinAndGet admin m2 conv1 code1 pw
      checkJoinAndGet admin m2 conv2 code2 pw
      checkJoinAndGet admin m2 conv3 code3 pw
      -- deletion works
      checkNoCode admin m1 convA codeA pw
      checkDelete admin m1 convB codeB pw
      waitForMigration domain counterName
      pure (code3, codeC)

    (code4, codeD) <- runCodensity (startDynamicBackend backend (conf "migration-to-postgresql" False)) $ \_ -> do
      -- code generation works
      code4 <- genCode admin conv4 pw
      codeD <- genCode admin convD pw
      -- joining works
      checkJoinAndGet admin m3 conv1 code1 pw
      checkJoinAndGet admin m3 conv2 code2 pw
      checkJoinAndGet admin m3 conv3 code3 pw
      checkJoinAndGet admin m3 conv4 code4 pw
      -- deletion works
      checkNoCode admin m1 convA codeA pw
      checkNoCode admin m1 convB codeB pw
      checkDelete admin m1 convC codeC pw
      pure (code4, codeD)

    runCodensity (startDynamicBackend backend (conf "postgresql" False)) $ \_ -> do
      -- code generation works
      code5 <- genCode admin conv5 pw
      -- joining works
      checkJoinAndGet admin m4 conv1 code1 pw
      checkJoinAndGet admin m4 conv2 code2 pw
      checkJoinAndGet admin m4 conv3 code3 pw
      checkJoinAndGet admin m4 conv4 code4 pw
      checkJoinAndGet admin m4 conv5 code5 pw
      -- deletion works
      checkNoCode admin m1 convA codeA pw
      checkNoCode admin m1 convB codeB pw
      checkNoCode admin m1 convC codeC pw
      checkDelete admin m1 convD codeD pw
      checkDelete admin m1 conv5 code5 pw
  where
    checkJoinAndGet admin user conv code pw = do
      joinWithCode user conv code
      getCode admin conv pw `shouldMatch` code
    checkDelete admin user conv (k, v) pw = do
      assertSuccess =<< deleteConversationCode admin conv
      checkNoCode admin user conv (k, v) pw
    checkNoCode admin user conv (k, v) pw = do
      assertStatus 404 =<< getConversationCode admin conv pw
      bindResponse (getJoinCodeConv user k v) $ \res -> do
        res.status `shouldMatchInt` 404
        res.json %. "label" `shouldMatch` "no-conversation-code"

-- HELPER

genCode :: (HasCallStack, MakesValue user, MakesValue conv) => user -> conv -> Maybe String -> App (String, String)
genCode user conv pw =
  bindResponse (postConversationCode user conv pw Nothing) $ \res -> do
    payload <- getJSON 201 res
    k <- payload %. "data.key" & asString
    v <- payload %. "data.code" & asString
    pure (k, v)

getCode :: (HasCallStack, MakesValue user, MakesValue conv) => user -> conv -> Maybe String -> App (String, String)
getCode user conv pw =
  bindResponse (getConversationCode user conv pw) $ \res -> do
    payload <- getJSON 200 res
    k <- payload %. "key" & asString
    v <- payload %. "code" & asString
    pure (k, v)

joinWithCode :: (HasCallStack, MakesValue user) => user -> Value -> (String, String) -> App ()
joinWithCode user conv (k, v) =
  bindResponse (getJoinCodeConv user k v) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "id" `shouldMatch` (objQidObject conv & objId)

conf :: String -> Bool -> ServiceOverrides
conf db runMigration =
  def
    { galleyCfg = setField "postgresMigration.conversationCodes" db,
      backgroundWorkerCfg = setField "migrateConversationCodes" runMigration
    }

counterName :: String
counterName = "^wire_conv_codes_migration_finished"
