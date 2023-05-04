module Test.MLS where

import SetupHelpers (createAndConnectUsers)
import Testlib.Prelude

-- TODO
testMixedUpgrade :: App ()
testMixedUpgrade = do
  own <- ownDomain
  [alice, bob] <- createAndConnectUsers [own, own]
  printJSON alice
  printJSON bob
  pure ()

-- testMixedUpgrade :: TestM ()
-- testMixedUpgrade = do
--   [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)

--   runMLSTest $ do
--     [alice1] <- traverse createMLSClient [alice]

--     qcnv <-
--       cnvQualifiedId
--         <$> liftTest
--           ( postConvQualified (qUnqualified alice) Nothing defNewProteusConv {newConvQualifiedUsers = [bob]}
--               >>= responseJsonError
--           )

--     putConversationProtocol (qUnqualified alice) (ciClient alice1) qcnv ProtocolMixedTag
--       !!! const 200 === statusCode

--     conv <-
--       responseJsonError
--         =<< getConvQualified (qUnqualified alice) qcnv
--           <!! const 200 === statusCode

--     mlsData <- assertMixedProtocol conv

--     liftIO $ assertEqual "" (cnvmlsEpoch mlsData) (Epoch 0)

--     putConversationProtocol (qUnqualified alice) (ciClient alice1) qcnv ProtocolMixedTag
--       !!! const 200 === statusCode

-- testMixedAddClients :: TestM ()
-- testMixedAddClients = do
--   [alice, bob, charlie] <- createAndConnectUsers (replicate 3 Nothing)

--   runMLSTest $ do
--     clients@[alice1, bob1, charlie1] <- traverse createMLSClient [alice, bob, charlie]
--     traverse_ uploadNewKeyPackage clients

--     -- alice creates conv
--     qcnv <-
--       cnvQualifiedId
--         <$> liftTest
--           ( postConvQualified (qUnqualified alice) Nothing defNewProteusConv {newConvQualifiedUsers = [bob, charlie]}
--               >>= responseJsonError
--           )

--     -- bob upgrades to mixed
--     putConversationProtocol (qUnqualified bob) (ciClient bob1) qcnv ProtocolMixedTag
--       !!! const 200 === statusCode

--     conv <-
--       responseJsonError
--         =<< getConvQualified (qUnqualified alice) qcnv
--           <!! const 200 === statusCode
--     void $ assertMixedProtocol conv

--     -- bob adds all client of alice and charlie
--     (_gid, _) <- setupMLSGroupWithConv (pure conv) bob1
--     commit <- createAddCommit bob1 [alice, charlie]
--     welcome <- assertJust (mpWelcome commit)
--     mlsBracket [alice1, charlie1] $ \wss -> do
--       void $ sendAndConsumeCommitBundle commit
--       for_ (zip [alice1, charlie1] wss) $ \(c, ws) ->
--         WS.assertMatch (5 # Second) ws $
--           wsAssertMLSWelcome (cidQualifiedUser c) welcome

--     -- charlie sends a Proteus message
--     let msgs =
--           [ (qUnqualified alice, ciClient alice1, toBase64Text "ciphertext-to-alice"),
--             (qUnqualified bob, ciClient bob1, toBase64Text "ciphertext-to-bob")
--           ]
--     liftTest $
--       postOtrMessage id (qUnqualified charlie) (ciClient charlie1) (qUnqualified qcnv) msgs !!! do
--         const 201 === statusCode
--         assertMismatch [] [] []
