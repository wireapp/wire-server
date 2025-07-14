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

import Data.Aeson qualified as Aeson
import Data.Id
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.These.Combinators
import Gundeck.Push
import Gundeck.Push.Websocket as Web (bulkPush)
import Imports
import MockGundeck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Internal.Notification
import Wire.API.Presence
import Wire.API.Push.V2
import Wire.API.User.Client
import Wire.Arbitrary

tests :: TestTree
tests =
  testGroup
    "push"
    [ testGroup
        "bulkpush"
        [ testProperty "web sockets" webBulkPushProps,
          testProperty "native pushes" pushAllProps
        ],
      testGroup "splitPush" [testProperty "rabbitmq pushes" splitPushActualRecipients]
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
                        RecipientClientsTemporaryOnly -> Set.empty
                   in Set.map (\c -> (r._recipientId, c.clientId)) clients
              )
              cassandraPush._pushRecipients

      actualTempRabbitMqRecipients :: Set UserId =
        flip foldMap mRabbitMqPush $ \rabbitmqPush ->
          Set.map (\r -> r._recipientId) rabbitmqPush._pushRecipients

      actualRabbitMqRecipients :: Set (UserId, ClientId) =
        flip foldMap mRabbitMqPush $ \rabbitmqPush ->
          Set.unions $
            Set.map
              ( \(r :: Recipient) ->
                  let clients = case r._recipientClients of
                        RecipientClientsAll -> allRabbitMqClientsFor r._recipientId
                        RecipientClientsSome cids -> Set.filter (\c -> c.clientId `elem` cids) $ allRabbitMqClientsFor r._recipientId
                        RecipientClientsTemporaryOnly -> Set.empty
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
                      RecipientClientsTemporaryOnly -> Set.empty
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

      expectedTempRabbitMqRecipients =
        Set.map (._recipientId) p.push._pushRecipients
   in counterexample ("actualRecipients: " <> show actualRabbitMqRecipients <> "\nallExpectedRecipients: " <> show allExpectedPushRecipients) $
        actualRabbitMqRecipients `Set.isSubsetOf` allExpectedPushRecipients
          .&&. actualTempRabbitMqRecipients === expectedTempRabbitMqRecipients
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
          RecipientClientsTemporaryOnly -> arbitrary
        pure (r._recipientId, clients)

      arbitraryClientWithId :: ClientId -> Gen Client
      arbitraryClientWithId cid = (\c -> c {clientId = cid} :: Client) <$> arbitrary
