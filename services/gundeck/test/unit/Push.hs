{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Push where

import Control.Lens hiding (united)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Id
import Data.IntMultiSet qualified as MSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.These.Combinators
import Gundeck.Push
import Gundeck.Push.Websocket as Web (bulkPush)
import Imports
import MockGundeck
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Random (mkQCGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.Internal.Notification
import Wire.API.Presence
import Wire.API.Push.V2
import Wire.API.User.Client
import Wire.Arbitrary
import Wire.WebPushStore (WebPushAddress (..))

tests :: TestTree
tests =
  testGroup
    "push"
    [ testGroup
        "bulkpush"
        [ testProperty "web sockets" webBulkPushProps,
          testProperty "native pushes" pushAllProps
        ],
      testGroup "splitPush" [testProperty "rabbitmq pushes" splitPushActualRecipients],
      testGroup
        "web push flow"
        [ testCase "web-only user (no native addr) receives a web push" webOnlyUserGetsWebPush,
          testCase "both-subscribed offline user receives both native and web" bothSubscribedOfflineGetsBoth,
          testCase "websocket-served client does not receive a web push" wsServedSkipsWebPush,
          testCase "origin client does not receive a web push" originClientSkipsWebPush,
          testCase "transient push skips web push" transientPushSkipsWebPush
        ]
    ]

mkEnv :: (Pretty MockEnv -> Property) -> Positive Int -> Property
mkEnv prop (Positive len) = forAllShrink (Pretty <$> resize len genMockEnv) (shrinkPretty shrinkMockEnv) prop

webBulkPushProps :: Positive Int -> Property
webBulkPushProps plen@(Positive len) = mkEnv mkNotifs plen
  where
    mkNotifs :: Pretty MockEnv -> Property
    mkNotifs (Pretty env) =
      forAllShrink
        (Pretty <$> resize len (genNotifs env))
        (shrinkPretty shrinkNotifs)
        (webBulkPushProp env)

webBulkPushProp :: MockEnv -> Pretty [(Notification, [Presence])] -> Property
webBulkPushProp env (Pretty notifs) =
  counterexample "^ environment, notifications\n" $
    conjoin props
  where
    (realout, realst) = runMockGundeck env $ Web.bulkPush notifs
    (mockout, mockst) = runMockGundeck env $ mockBulkPush notifs
    props =
      [ realst === mockst,
        sort realout === sort mockout
      ]

pushAllProps :: Positive Int -> Property
pushAllProps plen@(Positive len) = mkEnv mkPushes plen
  where
    mkPushes :: Pretty MockEnv -> Property
    mkPushes (Pretty env) =
      forAllShrink
        (Pretty <$> resize len (genPushes env))
        (shrinkPretty shrinkPushes)
        (pushAllProp env)

pushAllProp :: MockEnv -> Pretty [Push] -> Property
pushAllProp env (Pretty pushes) =
  counterexample "^ environment, pushes\n" $
    conjoin props
  where
    ((), realst) = runMockGundeck env (pushAll pushes)
    ((), mockst) = runMockGundeck env (mockPushAll pushes)
    props =
      [ (Aeson.eitherDecode . Aeson.encode) pushes === Right pushes,
        (Aeson.eitherDecode . Aeson.encode) env === Right env,
        counterexample "real vs. mock:" $ realst === mockst
      ]

splitPushActualRecipients :: PushWithUserClients -> Property
splitPushActualRecipients p =
  let pushes = splitPush p.userClients p.push
      mRabbitMqPush :: Maybe Push = justHere pushes
      mCassandraPush :: Maybe Push = justThere pushes

      clientsFor :: UserId -> Set Client
      clientsFor uid = Map.findWithDefault mempty uid p.userClients.userClientsFull

      allRabbitMqClientsFor :: UserId -> Set Client
      allRabbitMqClientsFor =
        Set.filter supportsConsumableNotifications . clientsFor

      allCassandraClientsFor :: UserId -> Set Client
      allCassandraClientsFor =
        Set.filter (not . supportsConsumableNotifications) . clientsFor

      actualCassandraRecipients :: Set (UserId, ClientId)
      actualCassandraRecipients =
        flip foldMap mCassandraPush $ \cassandraPush ->
          Set.unions $
            Set.map
              ( \(r :: Recipient) ->
                  let clients = case r._recipientClients of
                        RecipientClientsAll -> allCassandraClientsFor r._recipientId
                        RecipientClientsSome cids -> Set.filter (\c -> c.clientId `elem` cids) $ allCassandraClientsFor r._recipientId
                   in Set.map (\c -> (r._recipientId, c.clientId)) clients
              )
              cassandraPush._pushRecipients

      actualRabbitMqRecipients :: Set (UserId, ClientId) =
        flip foldMap mRabbitMqPush $ \rabbitmqPush ->
          Set.unions $
            Set.map
              ( \(r :: Recipient) ->
                  let clients = case r._recipientClients of
                        RecipientClientsAll -> allRabbitMqClientsFor r._recipientId
                        RecipientClientsSome cids -> Set.filter (\c -> c.clientId `elem` cids) $ allRabbitMqClientsFor r._recipientId
                   in Set.map (\c -> (r._recipientId, c.clientId)) clients
              )
              rabbitmqPush._pushRecipients

      allExpectedPushRecipients :: Set (UserId, ClientId) =
        Set.unions $
          Set.map
            ( \(r :: Recipient) ->
                let clients = case r._recipientClients of
                      RecipientClientsAll -> Set.map (.clientId) $ clientsFor r._recipientId
                      RecipientClientsSome cids -> Set.fromList $ Imports.toList cids
                 in Set.map (r._recipientId,) clients
            )
            p.push._pushRecipients

      (expectedRabbitMqRecipients, expectedCassandraRecipients) =
        Set.partition
          ( \(u, c) ->
              let rmqClients = Set.map (.clientId) $ allRabbitMqClientsFor u
               in Set.member c rmqClients
          )
          allExpectedPushRecipients
   in counterexample ("actualRecipients: " <> show actualRabbitMqRecipients <> "\nallExpectedRecipients: " <> show allExpectedPushRecipients) $
        actualRabbitMqRecipients `Set.isSubsetOf` allExpectedPushRecipients
          .&&. actualCassandraRecipients === expectedCassandraRecipients
          .&&. actualRabbitMqRecipients === expectedRabbitMqRecipients

data PushWithUserClients = PushWithUserClients {push :: Push, userClients :: UserClientsFull}
  deriving (Show, Eq)

instance Arbitrary PushWithUserClients where
  arbitrary = do
    ps <- arbitrary
    userClients <- traverse userClientsForRecipient $ Set.toList ps._pushRecipients
    pure $ PushWithUserClients ps (UserClientsFull $ Map.fromList userClients)
    where
      userClientsForRecipient :: Recipient -> Gen (UserId, (Set Client))
      userClientsForRecipient r = do
        clients <- case r._recipientClients of
          RecipientClientsSome cids -> do
            specifiedClients <- Set.fromList . Imports.toList <$> traverse arbitraryClientWithId cids
            extraClientIds <- Set.filter (`notElem` cids) <$> setOf' arbitrary
            extraClients <- Set.fromList <$> traverse arbitraryClientWithId (Set.toList extraClientIds)
            pure $ specifiedClients <> extraClients
          RecipientClientsAll -> do
            extraClientIds <- setOf' arbitrary
            Set.fromList <$> traverse arbitraryClientWithId (Set.toList extraClientIds)
        pure (r._recipientId, clients)

      arbitraryClientWithId :: ClientId -> Gen Client
      arbitraryClientWithId cid = (\c -> c {clientId = cid} :: Client) <$> arbitrary

-- | A throwaway 'Payload' for the explicit fixtures (the property test uses
-- 'genPayload'; here we just need a stable value to compare queues by).
fixturePayload :: Payload
fixturePayload = KeyMap.singleton "val" (Aeson.toJSON (42 :: Int)) :| []

-- | Deterministic generation helper: run a 'Gen' with a fixed seed so the
-- fixtures are reproducible across test runs.
generateWith :: Int -> Gen a -> a
generateWith seed g = unGen g (mkQCGen seed) 0

-- | Build a one-user, one-client 'MockEnv' with explicit reachability flags
-- and an optional list of web push subscriptions for that client. The
-- client is non-consumable (so it routes through 'pushAllLegacy', the only
-- pipeline that calls 'pushWebWithBudget').
singleUserEnv ::
  UserId ->
  ClientId ->
  -- | websocket-reachable
  Bool ->
  -- | native-reachable (has a native push address)
  Bool ->
  -- | web push subscriptions for this (user, client)
  [WebPushAddress] ->
  MockEnv
singleUserEnv uid cid isWsReachable isNativeReachable webSubs =
  -- The client is generated via @arbitrary@ then pinned to @cid@ and
  -- stripped of @clientCapabilities@. Empty capabilities means
  -- @supportsConsumableNotifications@ is 'False', so the client is
  -- guaranteed non-consumable: 'splitPush' will route it to the legacy
  -- pipeline ('pushAllLegacy'), which is the only pipeline that calls
  -- 'pushWebWithBudget'. Without this hardening, a future change to the
  -- 'Arbitrary Client' instance could silently produce a consumable
  -- client under the fixed seed, breaking all 5 fixtures with a confusing
  -- "empty queue" failure.
  let client =
        generateWith 1 arbitrary
          & \c -> c {clientId = cid, clientCapabilities = mempty} :: Client
      nativeAddr = generateWith 2 (genProtoAddress uid cid)
      clientInfo =
        MockGundeck.ClientInfo
          { _ciClient = client,
            _ciNativeAddress = if isNativeReachable then Just (nativeAddr, True) else Nothing,
            _ciWSReachable = isWsReachable
          }
   in MockEnv
        { _meClientInfos = Map.singleton uid (Map.singleton cid clientInfo),
          _meWebSubscriptions = if null webSubs then mempty else Map.singleton uid webSubs
        }

-- | Build a 'WebPushAddress' for the given @(user, client)@ deterministically.
-- The @conn@ field is 'fakeConnId' of the client (matching what production
-- filtering by @pushConnections@ / @pushOriginConnection@ expects).
mkWebSub :: UserId -> ClientId -> WebPushAddress
mkWebSub uid cid = generateWith 3 (genWebPushAddress uid cid)

-- | A non-transient, non-'RouteDirect' push from a non-origin sender to one
-- recipient's client. The workhorse fixture for the explicit cases.
mkPushTo :: UserId -> ClientId -> Bool -> Push
mkPushTo uid cid transient =
  newPush
    Nothing
    (Set.singleton (Recipient uid RouteAny (RecipientClientsSome (cid :| []))))
    fixturePayload
    & pushTransient .~ transient

-- | Run a push through the production 'pushAll' pipeline and return the
-- resulting mock state. This exercises 'pushAllLegacy' -> 'pushWebWithBudget'
-- -> 'webTargets' -> 'mpaWebTargets' -> 'mpaPushWeb'.
runProduction :: MockEnv -> Push -> MockState
runProduction env psh = snd (runMockGundeck env (pushAll [psh]))

-- | Assert the web-push queue contains exactly the given @(user, client)@
-- keys with the given delivery counts.
assertWebPushQueue :: MockState -> [((UserId, ClientId), Int)] -> Assertion
assertWebPushQueue st expected =
  let actualCounts = [(k, MSet.size v) | (k, v) <- Map.toList (st ^. msWebPushQueue)]
   in actualCounts @?= expected

-- | Assert the native-push queue contains exactly the given @(user, client)@
-- keys with the given delivery counts.
assertNativeQueue :: MockState -> [((UserId, ClientId), Int)] -> Assertion
assertNativeQueue st expected =
  let actualCounts = [(k, MSet.size v) | (k, v) <- Map.toList (st ^. msNativeQueue)]
   in actualCounts @?= expected

-- | Fixed UUIDs so the assertions have readable output on failure.
testUid :: Int -> UserId
testUid 1 = Id (read "00000000-0000-0000-0000-000000000001")
testUid 2 = Id (read "00000000-0000-0000-0000-000000000002")
testUid 3 = Id (read "00000000-0000-0000-0000-000000000003")
testUid 4 = Id (read "00000000-0000-0000-0000-000000000004")
testUid 5 = Id (read "00000000-0000-0000-0000-000000000005")
testUid _ = error "testUid: only 1-5 defined"

webOnlyUserGetsWebPush :: Assertion
webOnlyUserGetsWebPush = do
  let uid = testUid 1
      cid = ClientId 1
      env = singleUserEnv uid cid False False [mkWebSub uid cid]
      psh = mkPushTo uid cid False
      st = runProduction env psh
  -- Native queue empty (no native address); web push queue has one delivery.
  assertNativeQueue st []
  assertWebPushQueue st [((uid, cid), 1)]
  -- Cross-check: production and mock agree on the whole state.
  let mockSt = snd (runMockGundeck env (mockPushAll [psh]))
  st @?= mockSt

bothSubscribedOfflineGetsBoth :: Assertion
bothSubscribedOfflineGetsBoth = do
  let uid = testUid 2
      cid = ClientId 2
      -- nativeReachable=True (has native addr), wsReachable=False (offline):
      -- client gets BOTH native and web push.
      env = singleUserEnv uid cid False True [mkWebSub uid cid]
      psh = mkPushTo uid cid False
      st = runProduction env psh
  assertNativeQueue st [((uid, cid), 1)]
  assertWebPushQueue st [((uid, cid), 1)]
  let mockSt = snd (runMockGundeck env (mockPushAll [psh]))
  st @?= mockSt

wsServedSkipsWebPush :: Assertion
wsServedSkipsWebPush = do
  let uid = testUid 3
      cid = ClientId 3
      -- wsReachable=True: WS delivers the notification; the client is in
      -- 'dontPush' (via 'alreadySentClients') and web push must not fire.
      env = singleUserEnv uid cid True False [mkWebSub uid cid]
      psh = mkPushTo uid cid False
      st = runProduction env psh
  assertWebPushQueue st []
  let mockSt = snd (runMockGundeck env (mockPushAll [psh]))
  st @?= mockSt

originClientSkipsWebPush :: Assertion
originClientSkipsWebPush = do
  let uid = testUid 4
      cid = ClientId 4
      env = singleUserEnv uid cid False False [mkWebSub uid cid]
      -- Mark this user as the originator of the push.
      psh =
        mkPushTo uid cid False
          & pushOrigin ?~ uid
          & pushOriginConnection ?~ fakeConnId cid
      st = runProduction env psh
  -- Origin connection is filtered out by 'webTargets' (matches native).
  assertWebPushQueue st []
  let mockSt = snd (runMockGundeck env (mockPushAll [psh]))
  st @?= mockSt

transientPushSkipsWebPush :: Assertion
transientPushSkipsWebPush = do
  let uid = testUid 5
      cid = ClientId 5
      env = singleUserEnv uid cid False False [mkWebSub uid cid]
      psh = mkPushTo uid cid True -- transient
      st = runProduction env psh
  -- Transient pushes skip web push (matches native semantics).
  assertWebPushQueue st []
  let mockSt = snd (runMockGundeck env (mockPushAll [psh]))
  st @?= mockSt
