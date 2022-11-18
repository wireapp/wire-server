{-# LANGUAGE RecordWildCards #-}
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
{-# LANGUAGE TemplateHaskell #-}

module Wire.API.MLS.Proposal where

import Cassandra
import Control.Arrow
import Control.Lens (makePrisms)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Context
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data ProposalTag
  = AddProposalTag
  | UpdateProposalTag
  | RemoveProposalTag
  | PreSharedKeyProposalTag
  | ReInitProposalTag
  | ExternalInitProposalTag
  | AppAckProposalTag
  | GroupContextExtensionsProposalTag
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (Arbitrary) via GenericUniform ProposalTag

instance ParseMLS ProposalTag where
  parseMLS = parseMLSEnum @Word16 "proposal type"

instance SerialiseMLS ProposalTag where
  serialiseMLS = serialiseMLSEnum @Word16

data Proposal
  = AddProposal (RawMLS KeyPackage)
  | UpdateProposal KeyPackage
  | RemoveProposal KeyPackageRef
  | PreSharedKeyProposal PreSharedKeyID
  | ReInitProposal ReInit
  | ExternalInitProposal ByteString
  | AppAckProposal [MessageRange]
  | GroupContextExtensionsProposal [Extension]
  deriving stock (Eq, Show)

instance ParseMLS Proposal where
  parseMLS =
    parseMLS >>= \case
      AddProposalTag -> AddProposal <$> parseMLS
      UpdateProposalTag -> UpdateProposal <$> parseMLS
      RemoveProposalTag -> RemoveProposal <$> parseMLS
      PreSharedKeyProposalTag -> PreSharedKeyProposal <$> parseMLS
      ReInitProposalTag -> ReInitProposal <$> parseMLS
      ExternalInitProposalTag -> ExternalInitProposal <$> parseMLSBytes @Word16
      AppAckProposalTag -> AppAckProposal <$> parseMLSVector @Word32 parseMLS
      GroupContextExtensionsProposalTag ->
        GroupContextExtensionsProposal <$> parseMLSVector @Word32 parseMLS

mkRemoveProposal :: KeyPackageRef -> RawMLS Proposal
mkRemoveProposal ref = RawMLS bytes (RemoveProposal ref)
  where
    bytes = LBS.toStrict . runPut $ do
      serialiseMLS RemoveProposalTag
      serialiseMLS ref

serialiseAppAckProposal :: [MessageRange] -> Put
serialiseAppAckProposal mrs = do
  serialiseMLS AppAckProposalTag
  serialiseMLSVector @Word32 serialiseMLS mrs

mkAppAckProposal :: [MessageRange] -> RawMLS Proposal
mkAppAckProposal = uncurry RawMLS . (bytes &&& AppAckProposal)
  where
    bytes = LBS.toStrict . runPut . serialiseAppAckProposal

-- | Compute the proposal ref given a ciphersuite and the raw proposal data.
proposalRef :: CipherSuiteTag -> RawMLS Proposal -> ProposalRef
proposalRef cs =
  ProposalRef
    . csHash cs proposalContext
    . rmRaw

data PreSharedKeyTag = ExternalKeyTag | ResumptionKeyTag
  deriving (Bounded, Enum, Eq, Show)

instance ParseMLS PreSharedKeyTag where
  parseMLS = parseMLSEnum @Word16 "PreSharedKeyID type"

data PreSharedKeyID = ExternalKeyID ByteString | ResumptionKeyID Resumption
  deriving stock (Eq, Show)

instance ParseMLS PreSharedKeyID where
  parseMLS = do
    t <- parseMLS
    case t of
      ExternalKeyTag -> ExternalKeyID <$> parseMLSBytes @Word8
      ResumptionKeyTag -> ResumptionKeyID <$> parseMLS

data Resumption = Resumption
  { resUsage :: Word8,
    resGroupId :: GroupId,
    resEpoch :: Word64
  }
  deriving stock (Eq, Show)

instance ParseMLS Resumption where
  parseMLS =
    Resumption
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS

data ReInit = ReInit
  { riGroupId :: GroupId,
    riProtocolVersion :: ProtocolVersion,
    riCipherSuite :: CipherSuite,
    riExtensions :: [Extension]
  }
  deriving stock (Eq, Show)

instance ParseMLS ReInit where
  parseMLS =
    ReInit
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSVector @Word32 parseMLS

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

data ProposalOrRef = Inline Proposal | Ref ProposalRef
  deriving stock (Eq, Show)

instance ParseMLS ProposalOrRef where
  parseMLS =
    parseMLS >>= \case
      InlineTag -> Inline <$> parseMLS
      RefTag -> Ref <$> parseMLS

newtype ProposalRef = ProposalRef {unProposalRef :: ByteString}
  deriving stock (Eq, Show, Ord)

instance ParseMLS ProposalRef where
  parseMLS = ProposalRef <$> getByteString 16

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
