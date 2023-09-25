{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Client where

import API.Brig
import API.Brig qualified as API
import API.BrigInternal qualified as API
import API.Gundeck
import Control.Lens hiding ((.=))
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ProtoLens.Labels ()
import Data.String.Conversions
import Data.Time.Clock.POSIX
import Data.Time.Clock.System
import Data.Time.Format
import Data.Vector qualified as Vector
import SetupHelpers
import Testlib.Prelude hiding ((.=))
import Testlib.ResourcePool

testClientLastActive :: HasCallStack => App ()
testClientLastActive = do
  alice <- randomUser OwnDomain def
  c0 <- addClient alice def >>= getJSON 201
  cid <- c0 %. "id"

  -- newly created clients should not have a last_active value
  tm0 <- fromMaybe Null <$> lookupField c0 "last_active"
  tm0 `shouldMatch` Null

  now <- systemSeconds <$> liftIO getSystemTime

  -- fetching notifications updates last_active
  void $ getNotifications alice cid def

  c1 <- getClient alice cid >>= getJSON 200
  tm1 <- c1 %. "last_active" & asString
  ts1 <-
    round @Double
      . realToFrac
      . utcTimeToPOSIXSeconds
      <$> parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" tm1
  assertBool "last_active is earlier than expected" $ ts1 >= now

testListClientsIfBackendIsOffline :: HasCallStack => App ()
testListClientsIfBackendIsOffline = do
  resourcePool <- asks (.resourcePool)
  ownDomain <- asString OwnDomain
  otherDomain <- asString OtherDomain
  [ownUser1, ownUser2] <- createAndConnectUsers [OwnDomain, OtherDomain]
  ownClient1 <- objId $ bindResponse (API.addClient ownUser1 def) $ getJSON 201
  ownClient2 <- objId $ bindResponse (API.addClient ownUser2 def) $ getJSON 201
  ownUser1Id <- objId ownUser1
  ownUser2Id <- objId ownUser2

  let c1 =
        Object
          ( KeyMap.fromList
              [ ( Key.fromText $ cs ownUser1Id,
                  Array $ Vector.fromList [Object (KeyMap.fromList [(Key.fromText (cs "id"), String (cs ownClient1))])]
                )
              ]
          )
  let c2 =
        Object
          ( KeyMap.fromList
              [ ( Key.fromText $ cs ownUser2Id,
                  Array $ Vector.fromList [Object (KeyMap.fromList [(Key.fromText (cs "id"), String (cs ownClient2))])]
                )
              ]
          )
  let expectedResponse =
        Object
          ( KeyMap.fromList
              [ (Key.fromText $ cs ownDomain, c1),
                (Key.fromText $ cs otherDomain, c2)
              ]
          )

  let qualifiedUsers = [ownUser1, ownUser2]
  bindResponse (listUsersClients ownUser1 qualifiedUsers) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "qualified_user_map" `shouldMatch` expectedResponse

  runCodensity (acquireResources 1 resourcePool) $ \[downBackend] -> do
    downUser <- runCodensity (startDynamicBackend downBackend mempty) $ \_ -> do
      do
        let domains = [ownDomain, otherDomain, downBackend.berDomain]
        sequence_
          [ API.createFedConn x (API.FedConn y "full_search")
            | x <- domains,
              y <- domains,
              x /= y
          ]

      downUser <- randomUser downBackend.berDomain def
      _downClient <- objId $ bindResponse (API.addClient downUser def) $ getJSON 201
      connectUsers ownUser1 downUser
      connectUsers ownUser2 downUser

      pure (downUser)
    let qualifiedUsersIncludingDownUser = [ownUser1, ownUser2, downUser]
    bindResponse (listUsersClients ownUser1 qualifiedUsersIncludingDownUser) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "qualified_user_map" `shouldMatch` expectedResponse
