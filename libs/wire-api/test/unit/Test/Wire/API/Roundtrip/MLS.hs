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
import Wire.API.MLS.Extension
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "MLS roundtrip tests" $
    [ testRoundTrip @KeyPackageRef,
      testRoundTrip @RemoveProposalSender,
      testRoundTrip @RemoveProposalMessage,
      testRoundTrip @RemoveProposalPayload,
      testRoundTrip @AppAckProposalTest,
      testRoundTrip @ExtensionVector,
      testRoundTrip @PublicGroupStateTBS,
      testRoundTrip @PublicGroupState
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

newtype RemoveProposalMessage = RemoveProposalMessage (Message 'MLSPlainText)
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

newtype RemoveProposalTBS = RemoveProposalTBS (MessageTBS 'MLSPlainText)
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalTBS where
  arbitrary =
    fmap RemoveProposalTBS $
      MessageTBS KnownFormatTag
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (unRemoveProposalSender <$> arbitrary)
        <*> (unRemoveProposalPayload <$> arbitrary)

instance Arbitrary RemoveProposalMessage where
  arbitrary = do
    RemoveProposalTBS tbs <- arbitrary
    RemoveProposalMessage
      <$> (Message (mkRawMLS tbs) <$> arbitrary)

newtype RemoveProposalPayload = RemoveProposalPayload {unRemoveProposalPayload :: MessagePayload 'MLSPlainText}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalPayload where
  arbitrary = RemoveProposalPayload . ProposalMessage . mkRemoveProposal <$> arbitrary

newtype RemoveProposalSender = RemoveProposalSender
  {unRemoveProposalSender :: Sender 'MLSPlainText}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalSender where
  arbitrary = RemoveProposalSender . PreconfiguredSender <$> arbitrary

newtype AppAckProposalTest = AppAckProposalTest Proposal
  deriving newtype (ParseMLS, Eq, Show)

instance Arbitrary AppAckProposalTest where
  arbitrary = AppAckProposalTest . AppAckProposal <$> arbitrary

instance SerialiseMLS AppAckProposalTest where
  serialiseMLS (AppAckProposalTest (AppAckProposal mrs)) = serialiseAppAckProposal mrs
  serialiseMLS _ = serialiseAppAckProposal []

newtype ExtensionVector = ExtensionVector [Extension]
  deriving newtype (Arbitrary, Eq, Show)

instance ParseMLS ExtensionVector where
  parseMLS = ExtensionVector <$> parseMLSVector @Word32 (parseMLS @Extension)

instance SerialiseMLS ExtensionVector where
  serialiseMLS (ExtensionVector exts) = do
    serialiseMLSVector @Word32 serialiseMLS exts
