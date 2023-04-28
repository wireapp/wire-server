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
import Data.ByteString as B
import GHC.Records
import Imports
import Test.QuickCheck
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.ProposalTag
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.1-2
data Proposal
  = AddProposal (RawMLS KeyPackage)
  | UpdateProposal (RawMLS LeafNode)
  | RemoveProposal LeafIndex
  | PreSharedKeyProposal (RawMLS PreSharedKeyID)
  | ReInitProposal (RawMLS ReInit)
  | ExternalInitProposal ByteString
  | GroupContextExtensionsProposal [Extension]
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Proposal)

instance HasField "tag" Proposal ProposalTag where
  getField (AddProposal _) = AddProposalTag
  getField (UpdateProposal _) = UpdateProposalTag
  getField (RemoveProposal _) = RemoveProposalTag
  getField (PreSharedKeyProposal _) = PreSharedKeyProposalTag
  getField (ReInitProposal _) = ReInitProposalTag
  getField (ExternalInitProposal _) = ExternalInitProposalTag
  getField (GroupContextExtensionsProposal _) = GroupContextExtensionsProposalTag

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

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-8.4-6
data PreSharedKeyTag = ExternalKeyTag | ResumptionKeyTag
  deriving (Bounded, Enum, Eq, Show)

instance ParseMLS PreSharedKeyTag where
  parseMLS = parseMLSEnum @Word8 "PreSharedKeyID type"

instance SerialiseMLS PreSharedKeyTag where
  serialiseMLS = serialiseMLSEnum @Word8

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-8.4-6
data PreSharedKeyIDCore = ExternalKeyID ByteString | ResumptionKeyID Resumption
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PreSharedKeyIDCore)

instance ParseMLS PreSharedKeyIDCore where
  parseMLS = do
    t <- parseMLS
    case t of
      ExternalKeyTag -> ExternalKeyID <$> parseMLSBytes @VarInt
      ResumptionKeyTag -> ResumptionKeyID <$> parseMLS

instance SerialiseMLS PreSharedKeyIDCore where
  serialiseMLS (ExternalKeyID bs) = do
    serialiseMLS ExternalKeyTag
    serialiseMLSBytes @VarInt bs
  serialiseMLS (ResumptionKeyID r) = do
    serialiseMLS ResumptionKeyTag
    serialiseMLS r

data PreSharedKeyID = PreSharedKeyID
  { core :: PreSharedKeyIDCore,
    nonce :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PreSharedKeyID)

instance ParseMLS PreSharedKeyID where
  parseMLS = PreSharedKeyID <$> parseMLS <*> parseMLSBytes @VarInt

instance SerialiseMLS PreSharedKeyID where
  serialiseMLS psk = do
    serialiseMLS psk.core
    serialiseMLSBytes @VarInt psk.nonce

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-8.4-6
data Resumption = Resumption
  { usage :: Word8,
    groupId :: GroupId,
    epoch :: Word64
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
    serialiseMLS r.usage
    serialiseMLS r.groupId
    serialiseMLS r.epoch

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.1.5-2
data ReInit = ReInit
  { groupId :: GroupId,
    protocolVersion :: ProtocolVersion,
    cipherSuite :: CipherSuite,
    extensions :: [Extension]
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
    serialiseMLS ri.groupId
    serialiseMLS ri.protocolVersion
    serialiseMLS ri.cipherSuite
    serialiseMLSVector @VarInt serialiseMLS ri.extensions

data MessageRange = MessageRange
  { sender :: KeyPackageRef,
    firstGeneration :: Word32,
    lastGeneration :: Word32
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
    serialiseMLS sender
    serialiseMLS firstGeneration
    serialiseMLS lastGeneration

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.4-3
data ProposalOrRefTag = InlineTag | RefTag
  deriving stock (Bounded, Enum, Eq, Show)

instance ParseMLS ProposalOrRefTag where
  parseMLS = parseMLSEnum @Word8 "ProposalOrRef type"

instance SerialiseMLS ProposalOrRefTag where
  serialiseMLS = serialiseMLSEnum @Word8

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.4-3
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
  deriving newtype (Arbitrary)

instance ParseMLS ProposalRef where
  parseMLS = ProposalRef <$> parseMLSBytes @VarInt

instance SerialiseMLS ProposalRef where
  serialiseMLS = serialiseMLSBytes @VarInt . unProposalRef

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
