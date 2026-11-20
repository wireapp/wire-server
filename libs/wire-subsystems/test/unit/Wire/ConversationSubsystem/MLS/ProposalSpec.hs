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
import Wire.API.MLS.CipherSuite (CipherSuiteTag (..))
import Wire.API.MLS.Credential
import Wire.API.MLS.Proposal
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.Errors (ConversationSubsystemError (..))
import Wire.ConversationSubsystem.MLS.Proposal

spec :: Spec
spec = do
  describe "applyProposals" do
    it "removes a leaf with a member row" do
      case runApply [RemoveProposal 1] of
        Right (Right (im, action)) -> do
          im `shouldBe` imFromList [(0, RegularClient alice1)]
          action.paRemove `shouldBe` cmSingleton bob1 1
          action.paRemoveUnknown `shouldBe` mempty
        _ -> expectationFailure "removal of a known leaf was rejected"

    it "records a Remove of a leaf without a member row, leaving the index map unchanged" do
      case runApply [RemoveProposal 2] of
        Right (Right (im, action)) -> do
          im `shouldBe` indexMap
          action.paRemove `shouldBe` mempty
          action.paRemoveUnknown `shouldBe` Set.singleton 2
        _ -> expectationFailure "removal of an unknown leaf was rejected"

    for_ [(1, "known"), (2, "unknown")] $ \(idx, kind) ->
      it ("rejects removing the same " <> kind <> " leaf twice") do
        isLeft (runApply [RemoveProposal idx, RemoveProposal idx]) `shouldBe` True

  describe "checkProposal" do
    it "accepts a standalone Remove of a leaf with a member row" do
      case runCheck (RemoveProposal 1) of
        Right (Right ()) -> pure ()
        _ -> expectationFailure "standalone removal of a known leaf was rejected"

    it "rejects a standalone Remove of a leaf without a member row" do
      case runCheck (RemoveProposal 2) of
        Right (Left ConversationSubsystemErrorMLSInvalidLeafNodeIndex) -> pure ()
        _ -> expectationFailure "standalone removal of an unknown leaf was not rejected"

runApply ::
  [Proposal] ->
  Either MLSProtocolError (Either ConversationSubsystemError (IndexMap, ProposalAction))
runApply proposals =
  run
    . runError
    . runError
    . runState indexMap
    $ applyProposals ciphersuite proposals

runCheck :: Proposal -> Either MLSProtocolError (Either ConversationSubsystemError ())
runCheck proposal =
  run
    . runError
    . runError
    $ checkProposal ciphersuite indexMap proposal

ciphersuite :: CipherSuiteTag
ciphersuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519

-- | Leaf 0 is alice1, leaf 1 is bob1; leaf 2 has no member row.
indexMap :: IndexMap
indexMap = imFromList [(0, RegularClient alice1), (1, RegularClient bob1)]

alice1 :: ClientIdentity
alice1 = mkClient "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa" 1

bob1 :: ClientIdentity
bob1 = mkClient "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb" 2

mkClient :: String -> Word64 -> ClientIdentity
mkClient uid cid =
  ClientIdentity
    { ciDomain = Domain "example.com",
      ciUser = Id (fromJust (UUID.fromString uid)),
      ciClient = ClientId cid
    }
