{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.MLS.Proposal where

import Cassandra
import Control.Lens (makePrisms)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString as B
import Imports
import Test.QuickCheck
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Context
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.ProposalTag
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data Proposal
  = AddProposal (RawMLS KeyPackage)
  | UpdateProposal (RawMLS LeafNode)
  | RemoveProposal Word32
  | PreSharedKeyProposal (RawMLS PreSharedKeyID)
  | ReInitProposal (RawMLS ReInit)
  | ExternalInitProposal ByteString
  | GroupContextExtensionsProposal [Extension]
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Proposal)

instance ParseMLS Proposal where
  parseMLS =
    parseMLS >>= \case
      AddProposalTag -> AddProposal <$> parseMLS
      UpdateProposalTag -> UpdateProposal <$> parseMLS
      RemoveProposalTag -> RemoveProposal <$> parseMLS
      PreSharedKeyProposalTag -> PreSharedKeyProposal <$> parseMLS
      ReInitProposalTag -> ReInitProposal <$> parseMLS
      ExternalInitProposalTag -> ExternalInitProposal <$> parseMLSBytes @VarInt
      GroupContextExtensionsProposalTag ->
        GroupContextExtensionsProposal <$> parseMLSVector @VarInt parseMLS

instance SerialiseMLS Proposal where
  serialiseMLS (AddProposal kp) = do
    serialiseMLS AddProposalTag
    serialiseMLS kp
  serialiseMLS (UpdateProposal ln) = do
    serialiseMLS UpdateProposalTag
    serialiseMLS ln
  serialiseMLS (RemoveProposal i) = do
    serialiseMLS RemoveProposalTag
    serialiseMLS i
  serialiseMLS (PreSharedKeyProposal k) = do
    serialiseMLS PreSharedKeyProposalTag
    serialiseMLS k
  serialiseMLS (ReInitProposal ri) = do
    serialiseMLS ReInitProposalTag
    serialiseMLS ri
  serialiseMLS (ExternalInitProposal ko) = do
    serialiseMLS ExternalInitProposalTag
    serialiseMLSBytes @VarInt ko
  serialiseMLS (GroupContextExtensionsProposal es) = do
    serialiseMLS GroupContextExtensionsProposalTag
    serialiseMLSVector @VarInt serialiseMLS es

-- | Compute the proposal ref given a ciphersuite and the raw proposal data.
proposalRef :: CipherSuiteTag -> RawMLS Proposal -> ProposalRef
proposalRef cs =
  ProposalRef
    . csHash cs proposalContext
    . flip RawMLS ()
    . rmRaw

data PreSharedKeyTag = ExternalKeyTag | ResumptionKeyTag
  deriving (Bounded, Enum, Eq, Show)

instance ParseMLS PreSharedKeyTag where
  parseMLS = parseMLSEnum @Word8 "PreSharedKeyID type"

instance SerialiseMLS PreSharedKeyTag where
  serialiseMLS = serialiseMLSEnum @Word8

data PreSharedKeyID = ExternalKeyID ByteString | ResumptionKeyID Resumption
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PreSharedKeyID)

instance ParseMLS PreSharedKeyID where
  parseMLS = do
    t <- parseMLS
    case t of
      ExternalKeyTag -> ExternalKeyID <$> parseMLSBytes @VarInt
      ResumptionKeyTag -> ResumptionKeyID <$> parseMLS

instance SerialiseMLS PreSharedKeyID where
  serialiseMLS (ExternalKeyID bs) = do
    serialiseMLS ExternalKeyTag
    serialiseMLSBytes @VarInt bs
  serialiseMLS (ResumptionKeyID r) = do
    serialiseMLS ResumptionKeyTag
    serialiseMLS r

data Resumption = Resumption
  { resUsage :: Word8,
    resGroupId :: GroupId,
    resEpoch :: Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Resumption)

instance ParseMLS Resumption where
  parseMLS =
    Resumption
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS

instance SerialiseMLS Resumption where
  serialiseMLS r = do
    serialiseMLS r.resUsage
    serialiseMLS r.resGroupId
    serialiseMLS r.resEpoch

data ReInit = ReInit
  { riGroupId :: GroupId,
    riProtocolVersion :: ProtocolVersion,
    riCipherSuite :: CipherSuite,
    riExtensions :: [Extension]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ReInit)

instance ParseMLS ReInit where
  parseMLS =
    ReInit
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSVector @VarInt parseMLS

instance SerialiseMLS ReInit where
  serialiseMLS ri = do
    serialiseMLS ri.riGroupId
    serialiseMLS ri.riProtocolVersion
    serialiseMLS ri.riCipherSuite
    serialiseMLSVector @VarInt serialiseMLS ri.riExtensions

data MessageRange = MessageRange
  { mrSender :: KeyPackageRef,
    mrFirstGeneration :: Word32,
    mrLastGeneration :: Word32
  }
  deriving stock (Eq, Show)

instance Arbitrary MessageRange where
  arbitrary = MessageRange <$> arbitrary <*> arbitrary <*> arbitrary

instance ParseMLS MessageRange where
  parseMLS =
    MessageRange
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS

instance SerialiseMLS MessageRange where
  serialiseMLS MessageRange {..} = do
    serialiseMLS mrSender
    serialiseMLS mrFirstGeneration
    serialiseMLS mrLastGeneration

data ProposalOrRefTag = InlineTag | RefTag
  deriving stock (Bounded, Enum, Eq, Show)

instance ParseMLS ProposalOrRefTag where
  parseMLS = parseMLSEnum @Word8 "ProposalOrRef type"

instance SerialiseMLS ProposalOrRefTag where
  serialiseMLS = serialiseMLSEnum @Word8

data ProposalOrRef = Inline Proposal | Ref ProposalRef
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ProposalOrRef)

instance ParseMLS ProposalOrRef where
  parseMLS =
    parseMLS >>= \case
      InlineTag -> Inline <$> parseMLS
      RefTag -> Ref <$> parseMLS

instance SerialiseMLS ProposalOrRef where
  serialiseMLS (Inline p) = do
    serialiseMLS InlineTag
    serialiseMLS p
  serialiseMLS (Ref r) = do
    serialiseMLS RefTag
    serialiseMLS r

newtype ProposalRef = ProposalRef {unProposalRef :: ByteString}
  deriving stock (Eq, Show, Ord, Generic)

instance ParseMLS ProposalRef where
  parseMLS = ProposalRef <$> getByteString 16

instance SerialiseMLS ProposalRef where
  serialiseMLS = putByteString . unProposalRef

instance Arbitrary ProposalRef where
  arbitrary = ProposalRef . B.pack <$> vectorOf 16 arbitrary

makePrisms ''ProposalOrRef

data ProposalOrigin
  = ProposalOriginClient
  | ProposalOriginBackend
  deriving (Eq)

instance Cql ProposalOrigin where
  ctype = Tagged IntColumn
  toCql = CqlInt . originToInt
  fromCql (CqlInt i) = intToOrigin i
  fromCql _ = Left "intToOrigin: unexptected data"

originToInt :: ProposalOrigin -> Int32
originToInt ProposalOriginClient = 0
originToInt ProposalOriginBackend = 1

intToOrigin :: Int32 -> Either String ProposalOrigin
intToOrigin 0 = pure ProposalOriginClient
intToOrigin 1 = pure ProposalOriginBackend
intToOrigin n = Left $ "intToOrigin: unexptected int constant: " <> show n
