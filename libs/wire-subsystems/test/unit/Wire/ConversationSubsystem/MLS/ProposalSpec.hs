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

module Wire.ConversationSubsystem.MLS.ProposalSpec (spec) where

import Data.Domain (Domain (..))
import Data.Id
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Test.Hspec
import Wire.API.Error.Galley (MLSProtocolError)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.Proposal
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.Errors (ConversationSubsystemError (..))
import Wire.ConversationSubsystem.MLS.Proposal

-- | Outcome of applying proposals: the error is reduced to whether it is the
-- invalid leaf index error, since the error types have no 'Eq' instance.
data Outcome
  = InvalidLeafNodeIndex
  | OtherError
  | Applied IndexMap ProposalAction

runApply :: UntrackedRemoval -> IndexMap -> [Proposal] -> Outcome
runApply untracked im proposals = runApplyThenAdd untracked im proposals []

-- | Apply proposals, then add the given members as an Add proposal would.
runApplyThenAdd :: UntrackedRemoval -> IndexMap -> [Proposal] -> [GroupMember] -> Outcome
runApplyThenAdd untracked im proposals added =
  case run . runError @MLSProtocolError . runError @ConversationSubsystemError . runState im $ do
    action <- applyProposals untracked MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519 proposals
    (action <>) . mconcat <$> traverse (addProposedClient . Left) added of
    Left _ -> OtherError
    Right (Left ConversationSubsystemErrorMLSInvalidLeafNodeIndex) -> InvalidLeafNodeIndex
    Right (Left _) -> OtherError
    Right (Right (im', action)) -> Applied im' action

shouldRejectLeafIndex :: Outcome -> Expectation
shouldRejectLeafIndex = \case
  InvalidLeafNodeIndex -> pure ()
  _ -> expectationFailure "expected mls-invalid-leaf-node-index"

spec :: Spec
spec = describe "Wire.ConversationSubsystem.MLS.Proposal" do
  let dom = Domain "example.com"

      mkUser :: String -> UserId
      mkUser s = Id (fromJust (UUID.fromString s))

      alice, bob, carl :: ClientIdentity
      alice = ClientIdentity dom (mkUser "00000000-0000-0000-0000-000000000001") (ClientId 1)
      bob = ClientIdentity dom (mkUser "00000000-0000-0000-0000-000000000002") (ClientId 2)
      carl = ClientIdentity dom (mkUser "00000000-0000-0000-0000-000000000003") (ClientId 3)

      indexMap :: [(LeafIndex, ClientIdentity)] -> IndexMap
      indexMap = imFromList . map (fmap RegularClient)

      allowing = AllowUntrackedRemoval

  describe "applyProposals" do
    let im = indexMap [(0, alice)]

    it "records the removal of a leaf without a member row" do
      case runApply allowing im [RemoveProposal 3] of
        Applied im' action -> do
          im' `shouldBe` im
          action.paRemoveUntracked `shouldBe` Set.singleton 3
          cmAssocs action.paRemove `shouldBe` []
        _ -> expectationFailure "expected the removal to be applied"

    it "still removes tracked leaves" do
      case runApply allowing im [RemoveProposal 0] of
        Applied im' action -> do
          im' `shouldBe` mempty
          cmAssocs action.paRemove `shouldBe` [(alice, 0)]
          action.paRemoveUntracked `shouldBe` mempty
        _ -> expectationFailure "expected the removal to be applied"

    it "rejects the removal of a leaf without a member row outside of commits" do
      shouldRejectLeafIndex (runApply RejectUntrackedRemoval im [RemoveProposal 3])

    it "rejects removing the same leaf twice" do
      shouldRejectLeafIndex (runApply allowing im [RemoveProposal 3, RemoveProposal 3])
      shouldRejectLeafIndex (runApply allowing im [RemoveProposal 0, RemoveProposal 0])

    it "adds a client on a leaf removed by the same commit" do
      case runApplyThenAdd allowing (indexMap [(0, alice), (1, bob)]) [RemoveProposal 0] [RegularClient carl] of
        Applied im' action -> do
          im' `shouldBe` indexMap [(0, carl), (1, bob)]
          cmAssocs (fst <$> action.paAdd) `shouldBe` [(carl, 0)]
        _ -> expectationFailure "expected the add to be applied"
