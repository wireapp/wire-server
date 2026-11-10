-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.ConversationStore.MLS.ReconcileSpec (spec) where

import Data.Bifunctor (first)
import Data.Domain (Domain (..))
import Data.Id
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.API.MLS.Credential
import Wire.ConversationStore.MLS.Reconcile
import Wire.ConversationStore.MLS.Types

spec :: Spec
spec = describe "Wire.ConversationStore.MLS.Reconcile" do
  let dom = Domain "example.com"
      dom2 = Domain "other.example.com"

      mkUser :: String -> UserId
      mkUser s = Id (fromJust (UUID.fromString s))

      user1, user2, user3 :: UserId
      user1 = mkUser "00000000-0000-0000-0000-000000000001"
      user2 = mkUser "00000000-0000-0000-0000-000000000002"
      user3 = mkUser "00000000-0000-0000-0000-000000000003"

      cid1, cid2 :: ClientId
      cid1 = ClientId 1
      cid2 = ClientId 2

      hist1, hist2 :: HistoryClientId
      hist1 = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000011"))
      hist2 = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000012"))

      leaf :: Int32 -> GroupMember -> (Int32, GroupMember)
      leaf i m = (i, m)

      -- Leaf indices are Int32 in the row shape but LeafIndex in the tree map.
      tree :: [(Int32, GroupMember)] -> IndexMap
      tree = imFromList . map (first fromIntegral)

  describe "regular member rows" do
    it "inserts a row for a tree leaf with no row (ghost client)" do
      let leaves = tree [leaf 0 (regular dom user1 cid1)]
          rows = []
      mlsReconcileOps rows [] leaves
        `shouldBe` MlsReconcileOps
          { roMemberInsert = [(dom, user1, cid1, 0)],
            roMemberUpdateLeaf = [],
            roMemberDelete = [],
            roHistoryInsert = [],
            roHistoryUpdateLeaf = [],
            roHistoryDelete = []
          }

    it "updates the leaf when a row points at a different leaf" do
      let leaves = tree [leaf 3 (regular dom user1 cid1)]
          rows = [(dom, user1, cid1, 0, False)]
          ops = mlsReconcileOps rows [] leaves
      ops.roMemberUpdateLeaf `shouldBe` [(dom, user1, cid1, 3)]
      ops.roMemberInsert `shouldBe` []
      ops.roMemberDelete `shouldBe` []

    it "does nothing when rows match the tree" do
      let leaves =
            tree
              [ leaf 0 (regular dom user1 cid1),
                leaf 1 (regular dom2 user2 cid2)
              ]
          rows =
            [ (dom, user1, cid1, 0, False),
              (dom2, user2, cid2, 1, False)
            ]
      mlsReconcileOps rows [] leaves
        `shouldBe` emptyOps

    it "deletes a non-pending row absent from the tree" do
      let leaves = tree [leaf 0 (regular dom user1 cid1)]
          rows =
            [ (dom, user1, cid1, 0, False),
              (dom, user2, cid2, 1, False)
            ]
      mlsReconcileOps rows [] leaves
        `shouldBe` emptyOps {roMemberDelete = [(dom, user2, cid2)]}

    it "ignores pending rows entirely" do
      let leaves = tree [leaf 0 (regular dom user1 cid1)]
          -- pending row for user2/cid2 at leaf 1: leaf is unoccupied in the
          -- tree (that's fine for pending rows) and must NOT be deleted.
          rows =
            [ (dom, user1, cid1, 0, False),
              (dom, user2, cid2, 1, True)
            ]
      mlsReconcileOps rows [] leaves
        `shouldBe` emptyOps

    it "does not update a pending row even if its leaf differs" do
      let leaves = tree [leaf 5 (regular dom user1 cid1)]
          rows =
            [ (dom, user1, cid1, 0, True)
            ]
      mlsReconcileOps rows [] leaves
        `shouldBe` emptyOps

    it "updates leaf of matching identity at a new index and keeps other rows" do
      let leaves =
            tree
              [ leaf 2 (regular dom user1 cid1),
                leaf 4 (regular dom2 user3 cid1)
              ]
          rows =
            [ (dom, user1, cid1, 0, False),
              (dom2, user3, cid1, 4, False),
              (dom, user2, cid2, 1, True)
            ]
          ops = mlsReconcileOps rows [] leaves
      ops.roMemberUpdateLeaf `shouldBe` [(dom, user1, cid1, 2)]
      ops.roMemberInsert `shouldBe` []
      ops.roMemberDelete `shouldBe` []

  describe "history client rows" do
    it "inserts a row for a tree history-client leaf with no row" do
      let leaves = tree [leaf 0 (HistoryClient hist1)]
          rows = []
      mlsReconcileOps [] rows leaves
        `shouldBe` emptyOps {roHistoryInsert = [(hist1, 0)]}

    it "updates the leaf when a history row points at a different leaf" do
      let leaves = tree [leaf 2 (HistoryClient hist1)]
          rows = [(hist1, 0, False)]
      mlsReconcileOps [] rows leaves
        `shouldBe` emptyOps {roHistoryUpdateLeaf = [(hist1, 2)]}

    it "deletes a non-pending history row absent from the tree" do
      let leaves = tree [leaf 0 (HistoryClient hist1)]
          rows = [(hist1, 0, False), (hist2, 1, False)]
      mlsReconcileOps [] rows leaves
        `shouldBe` emptyOps {roHistoryDelete = [hist2]}

    it "ignores pending history rows entirely" do
      let leaves = tree [leaf 0 (HistoryClient hist1)]
          rows = [(hist1, 0, False), (hist2, 1, True)]
      mlsReconcileOps [] rows leaves
        `shouldBe` emptyOps

  describe "combined" do
    it "handles regular and history leaves in one diff" do
      let leaves =
            tree
              [ leaf 0 (regular dom user1 cid1),
                leaf 1 (HistoryClient hist1),
                leaf 2 (regular dom2 user2 cid2)
              ]
          memberRows =
            [ (dom, user1, cid1, 0, False),
              -- stale leaf
              (dom2, user2, cid2, 5, False),
              -- ghost: absent
              (dom, user3, cid2, 3, False),
              -- pending: untouched
              (dom, user2, cid2, 4, True)
            ]
          historyRows =
            [ (hist1, 1, False),
              -- stale leaf
              (hist2, 6, True)
            ]
          ops = mlsReconcileOps memberRows historyRows leaves
      ops.roMemberInsert `shouldBe` []
      ops.roMemberUpdateLeaf `shouldBe` [(dom2, user2, cid2, 2)]
      ops.roMemberDelete `shouldBe` [(dom, user3, cid2)]
      ops.roHistoryInsert `shouldBe` []
      ops.roHistoryUpdateLeaf `shouldBe` []
      ops.roHistoryDelete `shouldBe` []
  where
    emptyOps =
      MlsReconcileOps
        { roMemberInsert = [],
          roMemberUpdateLeaf = [],
          roMemberDelete = [],
          roHistoryInsert = [],
          roHistoryUpdateLeaf = [],
          roHistoryDelete = []
        }

regular :: Domain -> UserId -> ClientId -> GroupMember
regular dom usr c = RegularClient (ClientIdentity dom usr c)
