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
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "MLS roundtrip tests" $
    [ testRoundTrip @KeyPackageRef,
      testRoundTrip @RemoveProposalSender,
      testRoundTrip @RemoveProposalMessage,
      testRoundTrip @RemoveProposalPayload
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

newtype RemoveProposalMessage = RemoveProposalMessage (MessageTBS 'MLSPlainText)
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalMessage where
  arbitrary =
    fmap RemoveProposalMessage $
      MessageTBS KnownFormatTag
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (unRemoveProposalSender <$> arbitrary)
        <*> (unRemoveProposalPayload <$> arbitrary)

newtype RemoveProposalPayload = RemoveProposalPayload {unRemoveProposalPayload :: MessagePayload 'MLSPlainText}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalPayload where
  arbitrary = RemoveProposalPayload . ProposalMessage . mkRemoveProposal <$> arbitrary

newtype RemoveProposalSender = RemoveProposalSender
  {unRemoveProposalSender :: Sender 'MLSPlainText}
  deriving newtype (ParseMLS, SerialiseMLS, Eq, Show)

instance Arbitrary RemoveProposalSender where
  arbitrary = RemoveProposalSender . PreconfiguredSender <$> arbitrary
