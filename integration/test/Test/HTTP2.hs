{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.HTTP2 where

import API.BrigInternal
import API.Galley
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding qualified as T
import GHC.Stack
import MLS.Util
import Network.HTTP.Client
import SetupHelpers hiding (deleteUser)
import System.Process
import System.Random
import Testlib.Prelude

-- This is based on the description in
-- https://wearezeta.atlassian.net/browse/WPB-4787

-- NOTE:
-- If you find this test failing, try the following before rerunning it. This will reset the DNS container to a known initial state
-- cp deploy/dockerephemeral/coredns-config/db.example.com-old-serial deploy/dockerephemeral/coredns-config/db.example.com
-- docker restart dockerephemeral-coredns-1

testHTTP2Blocking :: HasCallStack => App ()
testHTTP2Blocking = do
  startDynamicBackends [def, def, def] $ \[domainA, domainB, domainC] -> do
    -- Setup MLS for users on backends A and B
    [alice, bob] <- createAndConnectUsers [domainA, domainB]
    charlie <- randomUser domainC $ def {team = True}
    [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
    traverse_ uploadNewKeyPackage [bob1]
    void $ createNewGroup alice1
    void $ createAddCommit alice1 [alice, bob] >>= sendAndConsumeCommitBundle

    -- Setup a conversaion on domain C we can query for from domain B
    xid <- postConversation charlie defProteus >>= getJSON 201
    connectTwoUsers charlie bob
    bId <- bob %. "qualified_id"
    let addMember = addMembers charlie xid def {role = Just "wire_member", users = [bId]}
    bindResponse addMember $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "type" `shouldMatch` "conversation.member-join"
      resp.json %. "qualified_from" `shouldMatch` objQidObject charlie
      resp.json %. "qualified_conversation" `shouldMatch` objQidObject xid
      users <- resp.json %. "data.users" >>= asList
      addedUsers <- forM users (%. "qualified_id")
      addedUsers `shouldMatchSet` [bId]

    env <- ask
    result <- liftIO $ flip finally resetDNS $ do
      -- Change the target in CoreDNS
      setupDNS
      -- Send a message from backend B to backend A
      let sendFromB = do
            -- Give the blocking requests a head-start to fill up the channels
            threadDelay $ round @Double $ 3e6 -- 3 seconds
            -- Send a message from B to A
            runAppWithEnv env $
              withWebSockets [bob1, alice1] $ \(wsSender : wss) -> do
                mp <- createApplicationMessage bob1 "hello, alice"
                void . bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
                  resp.status `shouldMatchInt` 201
                for_ wss $ \ws -> do
                  -- Check that it was received on A's side
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
          -- CANNOT FAIL/THROW EXCEPTIONS.
          -- This failing will kill the concurrency of the other
          -- request loop, and we are expecting this to fail.
          -- Send requests from backend B to backend C to fill and
          -- block the HTTP2 connection.
          getFromC =
            try @HttpException $
              replicateConcurrently numRequests $ do
                -- slightly stagger all of these requests, but for less time than the delay in sendFromB
                -- This _should_ help avoid the thundering herd effect a minimal amount, and give the
                -- TVar checks a small amount of breathing space.
                offset <- randomRIO (0, round @Double 2e6)
                threadDelay offset
                -- Request something from domain C, it doesn't matter what so long as
                -- it goes through domainB's federator.
                runAppWithEnv env $
                  getConversation bob xid

      -- Run requests to different domains at the same time.
      -- One should be going to domainC, which we have already
      -- set up with an _invalid_ host, so that requests block and
      -- eventually timeout. The other request function should keep
      -- running actual requests at once to a different domain.
      race getFromC sendFromB
    case result of
      Left r -> assertFailure $ "Failing requests finished first:\n" <> show (take 5 <$> r)
      Right _ -> pure ()
  where
    -- Pick a number well above the value set in HTTP2.Client.Manager.Internal for the number
    -- of concurrent requests per HTTP2 connection. In this test we want to saturate the
    -- connections with blocked requests.
    numRequests :: Int
    numRequests = 200
    -- Swap from the original DNS one to a version that lists a non-routable
    -- IP for the d3 environment.
    swapToTest =
      -- Swap the DNS files around
      callProcess
        "cp"
        [ "deploy/dockerephemeral/coredns-config/db.example.com-http2-test",
          "deploy/dockerephemeral/coredns-config/db.example.com"
        ]
    -- Swap back to the original (with a new serial) DNS file
    swapToNormal =
      callProcess
        "cp"
        [ "deploy/dockerephemeral/coredns-config/db.example.com-new-serial",
          "deploy/dockerephemeral/coredns-config/db.example.com"
        ]
    -- Wait 10 seconds. This is to allow CoreDNS to update its database
    waitDNS = threadDelay $ round @Double $ 10 * 1e6
    resetDNS = do
      swapToNormal
      waitDNS
    setupDNS = do
      swapToTest
      waitDNS

-- Run hundreds of requests to the same host, all over HTTP2
-- and ensure that every request is successfully answered.
testHTTP2AllSuccessful :: HasCallStack => App ()
testHTTP2AllSuccessful = do
  alice <- randomUser OwnDomain def
  aliceId <- alice %. "qualified_id"
  -- create conversation with no users
  cid <- postConversation alice defProteus >>= getJSON 201
  bob <- randomUser OwnDomain def
  bobId <- bob %. "qualified_id"
  let addMember = addMembers alice cid def {role = Just "wire_member", users = [bobId]}
  bindResponse addMember $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "not-connected"
  connectTwoUsers alice bob
  bindResponse addMember $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "conversation.member-join"
    resp.json %. "qualified_from" `shouldMatch` objQidObject alice
    resp.json %. "qualified_conversation" `shouldMatch` objQidObject cid
    users <- resp.json %. "data.users" >>= asList
    addedUsers <- forM users (%. "qualified_id")
    addedUsers `shouldMatchSet` [bobId]

  env <- ask
  liftIO $
    replicateConcurrently_ 1000 $
      runAppWithEnv env $
        -- Query via federation
        bindResponse (getConversation bob cid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          mems <- resp.json %. "members.others" & asList
          mem <- assertOne mems
          mem %. "qualified_id" `shouldMatch` aliceId
          mem %. "conversation_role" `shouldMatch` "wire_admin"
