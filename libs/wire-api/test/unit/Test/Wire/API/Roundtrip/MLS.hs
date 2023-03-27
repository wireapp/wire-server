{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# OPTIONS_GHC -Wwarn #-}

module Test.Wire.API.Roundtrip.MLS (tests) where

import Data.Binary.Put
import Imports
import qualified Proto.Mls
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)
import Wire.API.ConverProtoLens
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.GroupInfoBundle
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "MLS roundtrip tests" $
    [ testRoundTrip @KeyPackageRef,
      testRoundTrip @ClientIdentity,
      testRoundTrip @TestPreconfiguredSender,
      testRoundTrip @RemoveProposalMessage,
      testRoundTrip @RemoveProposalPayload,
      testRoundTrip @ExtensionVector,
      testRoundTrip @PublicGroupStateTBS,
      testRoundTrip @PublicGroupState,
      testRoundTrip @Welcome,
      testRoundTrip @OpaquePublicGroupState,
      testRoundTrip @VarInt,
      testConvertProtoRoundTrip @Proto.Mls.GroupInfoBundle @GroupInfoBundle,
      testConvertProtoRoundTrip @Proto.Mls.CommitBundle @TestCommitBundle
    ]

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ParseMLS a, SerialiseMLS a, Eq a, Show a) =>
  T.TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show (runPut (serialiseMLS v))) $
        Right v === (decodeMLS . runPut . serialiseMLS) v

testConvertProtoRoundTrip ::
  forall p a.
  ( Arbitrary a,
    Typeable a,
    Show a,
    Show p,
    Eq a,
    ConvertProtoLens p a
  ) =>
  T.TestTree
testConvertProtoRoundTrip = testProperty (show (typeRep @a)) trip
  where
    trip (v :: a) =
      counterexample (show (toProtolens @p @a v)) $
        Right v === do
          let pa = toProtolens @p @a v
          fromProtolens @p @a pa

--------------------------------------------------------------------------------
-- auxiliary types

class ArbitrarySender a where
  arbitrarySender :: Gen Sender

class ArbitraryFramedContentData a where
  arbitraryFramedContentData :: Gen FramedContentData

class ArbitraryFramedContent a where
  arbitraryFramedContent :: Gen FramedContent

newtype MessageGenerator tbs = MessageGenerator {unMessageGenerator :: Message}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance ArbitraryFramedContent fc => Arbitrary (MessageGenerator fc) where
  arbitrary =
    fmap MessageGenerator $ do
      fc <- arbitraryFramedContent @fc
      mt <- case fc.sender of
        SenderMember _ -> Just <$> arbitrary
        _ -> pure Nothing
      Message
        <$> arbitrary
        <*> fmap
          MessagePublic
          ( PublicMessage (mkRawMLS fc)
              <$> (FramedContentAuthData <$> arbitrary <*> pure Nothing)
              <*> pure mt
          )

data FramedContentGenerator sender payload

instance
  ( ArbitrarySender sender,
    ArbitraryFramedContentData payload
  ) =>
  ArbitraryFramedContent (FramedContentGenerator sender payload)
  where
  arbitraryFramedContent =
    FramedContent
      <$> arbitrary
      <*> arbitrary
      <*> arbitrarySender @sender
      <*> arbitrary
      <*> arbitraryFramedContentData @payload

---

newtype RemoveProposalMessage = RemoveProposalMessage {unRemoveProposalMessage :: Message}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalMessage where
  arbitrary =
    RemoveProposalMessage
      <$> (unMessageGenerator <$> arbitrary @(MessageGenerator (FramedContentGenerator TestPreconfiguredSender RemoveProposalPayload)))

---

newtype RemoveProposalPayload = RemoveProposalPayload {unRemoveProposalPayload :: FramedContentData}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalPayload where
  arbitrary = RemoveProposalPayload . FramedContentProposal . mkRawMLS . RemoveProposal <$> arbitrary

instance ArbitraryFramedContentData RemoveProposalPayload where
  arbitraryFramedContentData = unRemoveProposalPayload <$> arbitrary

---

newtype TestPreconfiguredSender = TestPreconfiguredSender
  {unTestPreconfiguredSender :: Sender}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary TestPreconfiguredSender where
  arbitrary = TestPreconfiguredSender . SenderExternal <$> arbitrary

instance ArbitrarySender TestPreconfiguredSender where
  arbitrarySender = unTestPreconfiguredSender <$> arbitrary

---

newtype ExtensionVector = ExtensionVector [Extension]
  deriving newtype (Arbitrary, Eq, Show)

instance ParseMLS ExtensionVector where
  parseMLS = ExtensionVector <$> parseMLSVector @VarInt (parseMLS @Extension)

instance SerialiseMLS ExtensionVector where
  serialiseMLS (ExtensionVector exts) = do
    serialiseMLSVector @VarInt serialiseMLS exts

---

newtype TestCommitBundle = TestCommitBundle {unTestCommitBundle :: CommitBundle}
  deriving (Show, Eq)

-- | The commit bundle should contain a commit message, not a remove proposal
-- message. However defining MLS serialization for Commits and all nested types
-- seems overkill to test the commit bundle roundtrip
instance Arbitrary TestCommitBundle where
  arbitrary = do
    bundle <-
      CommitBundle
        <$> (mkRawMLS . unRemoveProposalMessage <$> arbitrary)
        <*> oneof [Just <$> (mkRawMLS <$> arbitrary), pure Nothing]
        <*> arbitrary
    pure (TestCommitBundle bundle)

instance ConvertProtoLens Proto.Mls.CommitBundle TestCommitBundle where
  fromProtolens = fmap TestCommitBundle . fromProtolens @Proto.Mls.CommitBundle @CommitBundle
  toProtolens = toProtolens . unTestCommitBundle
