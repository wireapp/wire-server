{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import Data.Map qualified as Map
import Data.Text qualified as T
import Debug.Trace qualified as T
import GHC.Stack
import Network.HTTP.Client
import Notifications
import SetupHelpers hiding (deleteUser)
import Test.Conversation
import Test.User
import Testlib.One2One (generateRemoteAndConvIdWithDomain)
import Testlib.Prelude
import Testlib.ResourcePool

-- Also test graceful shutdown. How will this handle the server shutting down, ensuring
-- that requests are waited on so that we aren't dropping requests midway.
--

testHTTP2 :: HasCallStack => App ()
testHTTP2 = do
  startDynamicBackends [def, def, def] $ \dynDomains -> do
    [domainA, domainB, domainC] <- pure dynDomains
    uidA <- randomUser domainA def {team = True}
    uidB <- randomUser domainB def {team = True}
    uidC <- randomUser domainC def {team = True}
    assertConnected uidA domainB domainC
    assertConnected uidB domainA domainC
    assertConnected uidC domainA domainB
    -- Build a fully federated conversation
    cid <- postConversation uidA defProteus >>= getJSON 201
    bId <- uidB %. "qualified_id"
    cId <- uidC %. "qualified_id"
    connectTwoUsers uidA uidB
    connectTwoUsers uidA uidC
    connectTwoUsers uidB uidC
    let addMember = addMembers uidA cid def {role = Just "wire_member", users = [bId, cId]}
    -- Set up a conversation so we can do federation requests
    bindResponse addMember $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "type" `shouldMatch` "conversation.member-join"
      resp.json %. "qualified_from" `shouldMatch` objQidObject uidA
      resp.json %. "qualified_conversation" `shouldMatch` objQidObject cid
      users <- resp.json %. "data.users" >>= asList
      addedUsers <- forM users (%. "qualified_id")
      addedUsers `shouldMatchSet` [bId, cId]

    -- Rewrite the service map for domainC
    modEnv <- asks $! adjustServiceMap domainC mangleDomainOther
    -- _ <- assertFailure $ show $ Map.lookup domainC modEnv.serviceMap
    local (const modEnv) $ do
      env <- ask
      -- Run requests to different domains at the same time.
      -- One should be going to OtherDomain, which we have already
      -- set up with an _invalid_ host, so that requests block and
      -- eventually timeout. The other request function should keep
      -- running actual requests at once to a different domain.
      result <-
        liftIO $
          race
            -- Run 10 failing requests at once.
            -- NO FAIL. This failing will kill the concurrency of the other
            -- request loop, and we are expecting this to fail.
            ( try @HttpException $
                replicateConcurrently 10 $
                  -- Request somethig from domain c, it doesn't matter what so long as
                  -- it goes through domainA's federator.

                  -- Request somethig from domain c, it doesn't matter what so long as
                  -- it goes through domainA's federator.
                  runAppWithEnv env $
                    getClientsQualified uidA domainC uidC
            )
            -- Run 10 successful requests at once, on a finite loop.
            ( do
                replicateM_ 10 $
                  replicateConcurrently_ 10 $
                    runAppWithEnv env $
                      getClientsQualified uidA domainB uidB
            )
      case result of
        Left r -> assertFailure $ "Failing requests finished first:\n" <> show r
        Right _ -> pure ()
  where
    -- This IP addresses is from the TEST-NET-1 reserved block
    -- and should never be routable. If it is, then someone is
    -- doing something bad/wrong.
    host :: String
    host = "192.0.2.1"
    mangleDomainOther :: ServiceMap -> ServiceMap
    mangleDomainOther ServiceMap {..} =
      ServiceMap
        { brig = brig {host = host},
          backgroundWorker = backgroundWorker {host = host},
          cannon = cannon {host = host},
          cargohold = cargohold {host = host},
          federatorInternal = federatorInternal {host = host},
          federatorExternal = federatorExternal {host = host},
          galley = galley {host = host},
          gundeck = gundeck {host = host},
          nginz = nginz {host = host},
          spar = spar {host = host},
          proxy = proxy {host = host},
          stern = stern {host = host}
        }
    adjustServiceMap :: String -> (ServiceMap -> ServiceMap) -> Env -> Env
    adjustServiceMap dom f e = e {serviceMap = Map.adjust f dom $ serviceMap e}
    assertConnected :: (HasCallStack, MakesValue user) => user -> String -> String -> App ()
    assertConnected u d d' =
      bindResponse
        (getFederationStatus u [d, d'])
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "fully-connected"
