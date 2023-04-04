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
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
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
      testRoundTrip @GroupInfoData,
      testRoundTrip @Welcome,
      testRoundTrip @VarInt
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

newtype RemoveProposalMessage = RemoveProposalMessage Message
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
