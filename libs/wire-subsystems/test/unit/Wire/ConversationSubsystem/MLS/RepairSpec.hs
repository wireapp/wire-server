-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.MLS.RepairSpec (spec) where

import Data.ByteString qualified as BS
import Data.Domain (Domain (..))
import Data.Id
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.MLS.GroupInfo (GroupInfo)
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.Serialisation (RawMLS, decodeMLS')
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.MLS.Repair

-- | Generated with mls-test-cli: client 1 of user ...01 creates group "foo"
-- and adds client 2 of user ...02, both on example.com; the GroupInfo is the
-- one for epoch 1 and carries the ratchet tree.
loadGroupInfo :: IO (RawMLS GroupInfo)
loadGroupInfo = do
  raw <- BS.readFile "test/resources/mls/group_info_two_members.mls"
  either (fail . Text.unpack) pure (decodeMLS' raw)

spec :: Spec
spec = describe "Wire.ConversationSubsystem.MLS.Repair" do
  gi <- runIO loadGroupInfo
  let dom = Domain "example.com"

      mkUser :: String -> UserId
      mkUser s = Id (fromJust (UUID.fromString s))

      alice, bob, carl :: ClientIdentity
      alice = ClientIdentity dom (mkUser "00000000-0000-0000-0000-000000000001") (ClientId 1)
      bob = ClientIdentity dom (mkUser "00000000-0000-0000-0000-000000000002") (ClientId 2)
      carl = ClientIdentity dom (mkUser "00000000-0000-0000-0000-000000000003") (ClientId 3)

      tree :: [(LeafIndex, ClientIdentity)] -> IndexMap
      tree = imFromList . map (fmap RegularClient)

      row :: ClientIdentity -> Int32 -> Bool -> (Domain, UserId, ClientId, Int32, Bool)
      row cid idx isPending = (cid.ciDomain, cid.ciUser, cid.ciClient, idx, isPending)

      -- What the stores build from the member-client rows.
      snapshot :: [(Domain, UserId, ClientId, Int32, Bool)] -> (IndexMap, ClientMap LeafIndex)
      snapshot rows = (mkIndexMapFromParts rows [], mkClientMap rows)

      matches :: [(Domain, UserId, ClientId, Int32, Bool)] -> IndexMap -> Bool
      matches rows = uncurry rowsMatchTree (snapshot rows)

      gid = GroupId "foo"
      epoch = Epoch 1
      leaves = tree [(0, alice), (1, bob)]

  describe "validatedGroupInfoDigest" do
    it "records a digest for the group's tree at the post-commit epoch" do
      validatedGroupInfoDigest gid epoch leaves gi `shouldSatisfy` isJust

    it "records no digest for another group" do
      validatedGroupInfoDigest (GroupId "bar") epoch leaves gi `shouldBe` Nothing

    it "records no digest for another epoch" do
      validatedGroupInfoDigest gid (Epoch 0) leaves gi `shouldBe` Nothing
      validatedGroupInfoDigest gid (Epoch 2) leaves gi `shouldBe` Nothing

    it "records no digest if the tree does not match the validated membership" do
      for_
        [ tree [(0, alice)],
          tree [(0, alice), (1, bob), (2, carl)],
          tree [(0, alice), (2, bob)],
          tree [(0, bob), (1, alice)],
          tree [(0, alice), (1, carl)]
        ]
        \wrongLeaves ->
          validatedGroupInfoDigest gid epoch wrongLeaves gi `shouldBe` Nothing

  describe "rowsMatchTree" do
    it "accepts rows that match the tree" do
      matches [row alice 0 False, row bob 1 False] leaves `shouldBe` True

    it "rejects a missing row" do
      matches [row alice 0 False] leaves `shouldBe` False

    it "rejects a row on the wrong leaf" do
      matches [row alice 0 False, row bob 2 False] leaves `shouldBe` False

    it "rejects an active row sharing a leaf with the tree's client" do
      let rows = [row alice 0 False, row carl 0 False]
      -- The index map collapses both rows into alice@0, as in the tree.
      fst (snapshot rows) `shouldBe` tree [(0, alice)]
      matches rows (tree [(0, alice)]) `shouldBe` False

    it "accepts a pending row sharing a leaf, in any row order" do
      matches [row alice 0 False, row carl 0 True] (tree [(0, alice)]) `shouldBe` True
      matches [row carl 0 True, row alice 0 False] (tree [(0, alice)]) `shouldBe` True

    it "accepts a pending row absent from the tree, as the reconcile ignores it" do
      matches [row alice 0 False, row bob 1 False, row carl 2 True] leaves `shouldBe` True

    it "accepts a pending row for a tree client, even on another leaf" do
      matches [row alice 0 False, row bob 2 True] leaves `shouldBe` True

    it "rejects an active row absent from the tree" do
      matches [row alice 0 False, row bob 1 False, row carl 2 False] leaves `shouldBe` False

    it "rejects history entries that differ from the tree" do
      let hist = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000011"))
          rows = [row alice 0 False]
          activeRows = mkClientMap rows
      rowsMatchTree (mkIndexMapFromParts rows [(hist, 1, False)]) activeRows (tree [(0, alice)]) `shouldBe` False
      rowsMatchTree (mkIndexMapFromParts rows []) activeRows (tree [(0, alice)] <> imFromList [(1, HistoryClient hist)]) `shouldBe` False
      rowsMatchTree (mkIndexMapFromParts rows [(hist, 1, False)]) activeRows (tree [(0, alice)] <> imFromList [(1, HistoryClient hist)]) `shouldBe` True
