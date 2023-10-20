{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.HTTP2 where

import API.Brig
import API.BrigInternal
import API.Galley
import API.GalleyInternal
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as Base64
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace qualified as T
import GHC.Stack
import MLS.Util
import Network.HTTP.Client
import Notifications
import SetupHelpers hiding (deleteUser)
import System.Process
import System.Random
import Test.Conversation
import Test.User
import Testlib.One2One (generateRemoteAndConvIdWithDomain)
import Testlib.Prelude
import Testlib.ResourcePool

-- Also test graceful shutdown. How will this handle the server shutting down, ensuring
-- that requests are waited on so that we aren't dropping requests midway.

-- This is based on the description in
-- https://wearezeta.atlassian.net/browse/WPB-4787
testHTTP2 :: HasCallStack => App ()
testHTTP2 = do
  startDynamicBackends [def, def, def] $ \[domainA, domainB, domainC] -> do
    -- uidA <- randomUser domainA def {team = True}
    -- uidB <- randomUser domainB def {team = True}
    -- uidC <- randomUser domainC def {team = True}
    -- Ensure that it is fully connected
    -- assertConnected uidA domainB domainC
    -- assertConnected uidB domainA domainC
    -- assertConnected uidC domainA domainB
    -- Create a conversation on A
    -- xid <- postConversation uidA defProteus >>= getJSON 201
    -- Add a user from B to the conversation.
    -- bId <- uidB %. "qualified_id"
    -- connectTwoUsers uidA uidB
    -- let addMember = addMembers uidA xid def {role = Just "wire_member", users = [bId]}
    -- -- Set up a conversation so we can do federation requests
    -- bindResponse addMember $ \resp -> do
    --   resp.status `shouldMatchInt` 200
    --   resp.json %. "type" `shouldMatch` "conversation.member-join"
    --   resp.json %. "qualified_from" `shouldMatch` objQidObject uidA
    --   resp.json %. "qualified_conversation" `shouldMatch` objQidObject xid
    --   users <- resp.json %. "data.users" >>= asList
    --   addedUsers <- forM users (%. "qualified_id")
    --   addedUsers `shouldMatchSet` [bId]

    -- _ <- assertFailure $ show $ Map.lookup domainC modEnv.serviceMap

    -- Setup MLS for users on backends A and B
    [alice, bob] <- createAndConnectUsers [domainA, domainB]
    charlie <- randomUser domainC def {team = True}
    [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
    traverse_ uploadNewKeyPackage [bob1]
    void $ createNewGroup alice1
    void $ createAddCommit alice1 [alice, bob] >>= sendAndConsumeCommitBundle

    env <- ask
    result <- liftIO $ flip finally resetDNS $ do
      -- Change the target in CoreDNS
      setupDNS
      -- Send a message from backend B to backend A
      let sendFromB = do
            threadDelay $ round @Double $ 5e6 -- Three seconds
            replicateM 10 $
              runAppWithEnv env $
                withWebSockets [bob1, alice1] $ \(wsSender : wss) -> do
                  mp <- createApplicationMessage bob1 "hello, alice"
                  void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
                    resp.status `shouldMatchInt` 201
                  for_ wss $ \ws -> do
                    n <- awaitMatch 3 (\n -> nPayload n %. "type" `isEqual` "conversation.mls-message-add") ws
                    nPayload n %. "data" `shouldMatch` T.decodeUtf8 (Base64.encode mp.message)
                  expectFailure (const $ pure ()) $
                    awaitMatch
                      3
                      ( \n ->
                          liftM2
                            (&&)
                            (nPayload n %. "type" `isEqual` "conversation.mls-message-add")
                            (nPayload n %. "data" `isEqual` T.decodeUtf8 (Base64.encode mp.message))
                      )
                      wsSender
          -- NO FAIL. This failing will kill the concurrency of the other
          -- request loop, and we are expecting this to fail.
          getFromC =
            try @HttpException $
              replicateConcurrently numRequests $ do
                -- slightly stagger all of these requests, but for less time than the delay in sendFromB
                offset <- randomRIO (0, round @Double 2e6)
                threadDelay offset
                -- Request somethig from domain c, it doesn't matter what so long as
                -- it goes through domainA's federator.
                runAppWithEnv env $
                  getClientsQualified alice domainC charlie

      -- Run requests to different domains at the same time.
      -- One should be going to domainC, which we have already
      -- set up with an _invalid_ host, so that requests block and
      -- eventually timeout. The other request function should keep
      -- running actual requests at once to a different domain.
      race getFromC sendFromB
    case result of
      Left r -> assertFailure $ "Failing requests finished first:\n" -- <> show (take 5 <$> r)
      Right _ -> pure ()
  where
    numRequests :: Int
    numRequests = 200
    assertConnected :: (HasCallStack, MakesValue user) => user -> String -> String -> App ()
    assertConnected u d d' =
      bindResponse
        (getFederationStatus u [d, d'])
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "fully-connected"
    -- Swap from the original DNS one to a version that lists a non-routable
    -- IP for the d3 environment.
    swapToTest =
      -- Swap the DNS files around
      callProcess
        "cp"
        [ "deploy/dockerephemeral/coredns-config/db.example.com-http2-test",
          "deploy/dockerephemeral/coredns-config/db.example.com"
        ]
    -- Swap back to the (almost) original DNS file, with the exception of the serial
    -- number, which is needed to convince CoreDNS that this really has changed and
    -- be delivered to requestors.
    swapToNormal =
      callProcess
        "cp"
        [ "deploy/dockerephemeral/coredns-config/db.example.com-new-serial",
          "deploy/dockerephemeral/coredns-config/db.example.com"
        ]
    -- Wait 5 seconds. This is to allow CoreDNS to update its database
    waitDNS = threadDelay $ round @Double $ 5 * 10e6
    resetDNS = do
      swapToNormal
      waitDNS
    setupDNS = do
      swapToTest
      waitDNS
