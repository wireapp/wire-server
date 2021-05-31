-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module API.Federation where

import API.Util
import Bilge
import Bilge.Assert
import qualified Cassandra as Cql
import Control.Lens hiding ((#))
import Data.Domain
import Data.Id (Id (..), randomId)
import Data.List1
import Data.Qualified (Qualified (..))
import Data.Time.Clock
import Data.Timeout (TimeoutUnit (..), (#))
import Data.UUID.V4 (nextRandom)
import qualified Galley.Data.Queries as Cql
import Galley.Types
import Imports
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley (GetConversationsRequest (..), GetConversationsResponse (..))
import qualified Wire.API.Federation.API.Galley as FedGalley

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "federation"
    [ test s "POST /federation/get-conversations : All Found" getConversationsAllFound,
      test s "POST /federation/get-conversations : Conversations user is not a part of are excluded from result" getConversationsNotPartOf,
      test
        s
        "POST /federation/update-conversation-memberships : Add local user"
        addLocalUser,
      test
        s
        "POST /federation/update-conversation-memberships : Notify local user"
        notifyLocalUser
    ]

getConversationsAllFound :: TestM ()
getConversationsAllFound = do
  -- FUTUREWORK: make alice / bob remote users
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [cnvId cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv1]) === fmap (map cnvId . convList) . responseJsonUnsafe
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <- responseJsonUnsafeWithMsg "conversation" <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  getConvs alice (Just $ Left [cnvId cnv2]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv2]) === fmap (map cnvId . convList) . responseJsonUnsafe
  -- get both

  fedGalleyClient <- view tsFedGalleyClient
  localDomain <- viewFederationDomain
  let aliceQualified = Qualified alice localDomain
  GetConversationsResponse cs <- FedGalley.getConversations fedGalleyClient (GetConversationsRequest aliceQualified [cnvId cnv1, cnvId cnv2])
  let c1 = find ((== cnvId cnv1) . cnvId) cs
  let c2 = find ((== cnvId cnv2) . cnvId) cs
  liftIO . forM_ [(cnv1, c1), (cnv2, c2)] $ \(expected, actual) -> do
    assertEqual
      "name mismatch"
      (Just $ cnvName expected)
      (cnvName <$> actual)
    assertEqual
      "self member mismatch"
      (Just . cmSelf $ cnvMembers expected)
      (cmSelf . cnvMembers <$> actual)
    assertEqual
      "other members mismatch"
      (Just [])
      ((\c -> cmOthers (cnvMembers c) \\ cmOthers (cnvMembers expected)) <$> actual)

getConversationsNotPartOf :: TestM ()
getConversationsNotPartOf = do
  -- FUTUREWORK: make alice / bob remote users
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [cnvId cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvId cnv1]) === fmap (map cnvId . convList) . responseJsonUnsafe

  fedGalleyClient <- view tsFedGalleyClient
  localDomain <- viewFederationDomain
  rando <- Id <$> liftIO nextRandom
  let randoQualified = Qualified rando localDomain
  GetConversationsResponse cs <- FedGalley.getConversations fedGalleyClient (GetConversationsRequest randoQualified [cnvId cnv1])
  liftIO $ assertEqual "conversation list not empty" [] cs

addLocalUser :: TestM ()
addLocalUser = do
  localDomain <- viewFederationDomain
  alice <- randomUser
  let qalice = Qualified alice localDomain
  let dom = Domain "bobland.example.com"
  bob <- randomId
  let qbob = Qualified bob dom
  conv <- randomId
  let qconv = Qualified conv dom
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cmu =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qbob,
            FedGalley.cmuConvId = qconv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuUsersAdd = [(qalice, roleNameWireMember)],
            FedGalley.cmuUsersRemove = []
          }
  FedGalley.updateConversationMemberships fedGalleyClient cmu
  cassState <- view tsCass
  convs <-
    Cql.runClient cassState
      . Cql.query Cql.selectUserRemoteConvs
      $ Cql.params Cql.Quorum (Identity alice)
  liftIO $ [(dom, conv)] @?= convs

notifyLocalUser :: TestM ()
notifyLocalUser = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomId
  charlie <- randomId
  conv <- randomId
  let bdom = Domain "bob.example.com"
      cdom = Domain "charlie.example.com"
      qbob = Qualified bob bdom
      qconv = Qualified conv bdom
      qcharlie = Qualified charlie cdom
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cmu =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qbob,
            FedGalley.cmuConvId = qconv,
            FedGalley.cmuAlreadyPresentUsers = [alice],
            FedGalley.cmuUsersAdd = [(qcharlie, roleNameWireMember)],
            FedGalley.cmuUsersRemove = []
          }
  WS.bracketR c alice $ \ws -> do
    FedGalley.updateConversationMemberships fedGalleyClient cmu
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertMemberJoinWithRole conv bob [charlie] roleNameWireMember
