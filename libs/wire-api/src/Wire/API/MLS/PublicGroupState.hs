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
{-# LANGUAGE RecordWildCards #-}

module Wire.API.MLS.PublicGroupState where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Swagger as S
import Imports
import Servant.API.ContentTypes
import Test.QuickCheck hiding (label)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Epoch
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Servant
import Wire.Arbitrary

data PublicGroupStateTBS = PublicGroupStateTBS
  { pgsVersion :: ProtocolVersion,
    pgsCipherSuite :: CipherSuite,
    pgsGroupId :: GroupId,
    pgsEpoch :: Epoch,
    pgsTreeHash :: ByteString,
    pgsInterimTranscriptHash :: ByteString,
    pgsConfirmedInterimTranscriptHash :: ByteString,
    pgsGroupContextExtensions :: ByteString,
    pgsOtherExtensions :: ByteString,
    pgsExternalPub :: ByteString,
    pgsSigner :: KeyPackageRef
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PublicGroupStateTBS)

instance ParseMLS PublicGroupStateTBS where
  parseMLS =
    PublicGroupStateTBS
      <$> label "pgsVersion" parseMLS
      <*> label "pgsCipherSuite" parseMLS
      <*> label "pgsGroupId" parseMLS
      <*> label "pgsEpoch" parseMLS
      <*> label "pgsTreeHash" (parseMLSBytes @Word8)
      <*> label "pgsInterimTranscriptHash" (parseMLSBytes @Word8)
      <*> label "pgsConfirmedInterimTranscriptHash" (parseMLSBytes @Word8)
      <*> label "pgsGroupContextExtensions" (parseMLSBytes @Word32)
      <*> label "pgsOtherExtensions" (parseMLSBytes @Word32)
      <*> label "pgsExternalPub" (parseMLSBytes @Word16)
      <*> label "pgsSigner" parseMLS

instance SerialiseMLS PublicGroupStateTBS where
  serialiseMLS (PublicGroupStateTBS {..}) = do
    serialiseMLS pgsVersion
    serialiseMLS pgsCipherSuite
    serialiseMLS pgsGroupId
    serialiseMLS pgsEpoch
    serialiseMLSBytes @Word8 pgsTreeHash
    serialiseMLSBytes @Word8 pgsInterimTranscriptHash
    serialiseMLSBytes @Word8 pgsConfirmedInterimTranscriptHash
    serialiseMLSBytes @Word32 pgsGroupContextExtensions
    serialiseMLSBytes @Word32 pgsOtherExtensions
    serialiseMLSBytes @Word16 pgsExternalPub
    serialiseMLS pgsSigner

data PublicGroupState = PublicGroupState
  { pgTBS :: RawMLS PublicGroupStateTBS,
    pgSignature :: ByteString
  }
  deriving stock (Eq, Show, Generic)

-- | A type that holds an MLS-encoded 'PublicGroupState' value via
-- 'serialiseMLS'.
newtype OpaquePublicGroupState = OpaquePublicGroupState
  {unOpaquePublicGroupState :: LBS.ByteString}

instance ParseMLS OpaquePublicGroupState where
  parseMLS = OpaquePublicGroupState <$> getRemainingLazyByteString

instance SerialiseMLS OpaquePublicGroupState where
  serialiseMLS (OpaquePublicGroupState bs) = put bs

instance S.ToSchema OpaquePublicGroupState where
  declareNamedSchema _ = pure (mlsSwagger "OpaquePublicGroupState")

instance MimeRender MLS OpaquePublicGroupState where
  mimeRender _ = unOpaquePublicGroupState

toOpaquePublicGroupState :: RawMLS PublicGroupState -> OpaquePublicGroupState
toOpaquePublicGroupState = OpaquePublicGroupState . LBS.fromStrict . rmRaw

instance Arbitrary PublicGroupState where
  arbitrary =
    PublicGroupState
      <$> (mkRawMLS <$> arbitrary)
      <*> arbitrary

instance ParseMLS PublicGroupState where
  parseMLS =
    PublicGroupState
      <$> label "pgTBS" parseMLS
      <*> label "pgSignature" (parseMLSBytes @Word16)

instance SerialiseMLS PublicGroupState where
  serialiseMLS PublicGroupState {..} = do
    serialiseMLS pgTBS
    serialiseMLSBytes @Word16 pgSignature
