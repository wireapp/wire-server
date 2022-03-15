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

module Wire.API.MLS.Proposal
  ( Resumption (..),
    PreSharedKeyID (..),
    ReInit (..),
    MessageRange (..),
    Proposal (..),
    ProposalOrRef (..),
  )
where

import Control.Lens (_1)
import Control.Lens.Prism (prism)
import Control.Lens.Type (Prism')
import Data.Binary
import Data.Binary.Get
import Data.Json.Util
import Data.Schema
import qualified Data.Swagger as S
import Imports
import Wire.API.Arbitrary
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

data PreSharedKeyTag = ExternalKeyTag | ResumptionKeyTag
  deriving (Bounded, Enum, Eq, Show)

instance ParseMLS PreSharedKeyTag where
  parseMLS = parseMLSEnum @Word16 "PreSharedKeyID type"

data Resumption = Resumption
  { resUsage :: Word8,
    resGroupId :: GroupId,
    resEpoch :: Word64
  }
  deriving stock (Eq, Show)
  deriving (S.ToSchema) via Schema Resumption

instance ParseMLS Resumption where
  parseMLS =
    Resumption
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS

instance ToSchema Resumption where
  schema =
    object "Resumption" $
      Resumption
        <$> resUsage .= field "usage" schema
        <*> resGroupId .= field "group_id" schema
        <*> resEpoch .= field "epoch" schema

data PreSharedKeyIDTag = ExternalKeyIDTag | ResumptionKeyIDTag
  deriving (Bounded, Enum, Eq)

preSharedKeyIDTagSchema :: ValueSchema NamedSwaggerDoc PreSharedKeyIDTag
preSharedKeyIDTagSchema =
  enum @Text "PreSharedKeyID Tag" $
    mconcat
      [ element "external-key-id" ExternalKeyIDTag,
        element "resumption-key-id" ResumptionKeyIDTag
      ]

data PreSharedKeyID = ExternalKeyID ByteString | ResumptionKeyID Resumption
  deriving stock (Eq, Show)
  deriving (S.ToSchema) via Schema PreSharedKeyID

instance ToSchema PreSharedKeyID where
  schema =
    object "PreSharedKeyID" $
      fromTagged <$> toTagged
        .= bind
          (fst .= field "tag" preSharedKeyIDTagSchema)
          (snd .= fieldOver _1 "value" untaggedSchema)
    where
      toTagged :: PreSharedKeyID -> (PreSharedKeyIDTag, PreSharedKeyID)
      toTagged i@(ExternalKeyID _) = (ExternalKeyIDTag, i)
      toTagged i@(ResumptionKeyID _) = (ResumptionKeyIDTag, i)

      fromTagged :: (PreSharedKeyIDTag, PreSharedKeyID) -> PreSharedKeyID
      fromTagged = snd

      untaggedSchema = dispatch $ \case
        ExternalKeyIDTag -> tag _ExternalKeyID bsSchema
        ResumptionKeyIDTag -> tag _ResumptionKeyID (unnamed schema)

instance ParseMLS PreSharedKeyID where
  parseMLS = do
    t <- parseMLS
    case t of
      ExternalKeyTag -> ExternalKeyID <$> parseMLSBytes @Word8
      ResumptionKeyTag -> ResumptionKeyID <$> parseMLS

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

instance ToSchema ReInit where
  schema =
    object "ReInit" $
      ReInit
        <$> riGroupId .= field "group-id" schema
        <*> riProtocolVersion .= field "protocol-version" schema
        <*> riCipherSuite .= field "cipher-suite" schema
        <*> riExtensions .= field "extensions" (array schema)

data MessageRange = MessageRange
  { mrSender :: KeyPackageRef,
    mrFirstGeneration :: Word32,
    mrLastGenereation :: Word32
  }
  deriving stock (Eq, Show)

instance ParseMLS MessageRange where
  parseMLS =
    MessageRange
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS

instance ToSchema MessageRange where
  schema =
    object "MessageRange" $
      MessageRange
        <$> mrSender .= field "sender" schema
        <*> mrFirstGeneration .= field "first-generation" schema
        <*> mrLastGenereation .= field "last-generation" schema

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

data Proposal
  = AddProposal KeyPackage
  | UpdateProposal KeyPackage
  | RemoveProposal KeyPackageRef
  | PreSharedKeyProposal PreSharedKeyID
  | ReInitProposal ReInit
  | ExternalInitProposal ByteString
  | AppAckProposal [MessageRange]
  | GroupContextExtensionsProposal [Extension]
  deriving stock (Eq, Show)
  deriving (S.ToSchema) via Schema Proposal

instance ToSchema Proposal where
  schema =
    named "Proposal" $
      tag _AddProposal (unnamed schema)
        <> tag _UpdateProposal (unnamed schema)
        <> tag _RemoveProposal (unnamed schema)
        <> tag _PreSharedKeyProposal (unnamed schema)
        <> tag _ReInitProposal (unnamed schema)
        <> tag _ExternalInitProposal bsSchema
        <> tag _AppAckProposal (array $ unnamed schema)
        <> tag _GroupContextExtensionsProposal (array $ unnamed schema)

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
  deriving stock (Eq, Show)

instance ParseMLS ProposalRef where
  parseMLS = ProposalRef <$> getByteString 16

bsSchema :: ValueSchema SwaggerDoc ByteString
bsSchema = Base64ByteString .= fmap fromBase64ByteString (unnamed schema)

--------------------------------------------------------------------------------
-- Prisms

_ExternalKeyID ::
  Prism' PreSharedKeyID ByteString
_ExternalKeyID =
  prism
    ExternalKeyID
    ( \keyID ->
        case keyID of
          ExternalKeyID extKeyID -> Right extKeyID
          _ -> Left keyID
    )
{-# INLINE _ExternalKeyID #-}

_ResumptionKeyID ::
  Prism' PreSharedKeyID Resumption
_ResumptionKeyID =
  prism
    ResumptionKeyID
    ( \keyID ->
        case keyID of
          ResumptionKeyID resKeyID -> Right resKeyID
          _ -> Left keyID
    )
{-# INLINE _ResumptionKeyID #-}

_AddProposal :: Prism' Proposal KeyPackage
_AddProposal =
  prism
    AddProposal
    ( \p ->
        case p of
          AddProposal addp -> Right addp
          _ -> Left p
    )
{-# INLINE _AddProposal #-}

_UpdateProposal :: Prism' Proposal KeyPackage
_UpdateProposal =
  prism
    UpdateProposal
    ( \p ->
        case p of
          UpdateProposal up -> Right up
          _ -> Left p
    )
{-# INLINE _UpdateProposal #-}

_RemoveProposal :: Prism' Proposal KeyPackageRef
_RemoveProposal =
  prism
    RemoveProposal
    ( \p ->
        case p of
          RemoveProposal rp -> Right rp
          _ -> Left p
    )
{-# INLINE _RemoveProposal #-}

_PreSharedKeyProposal :: Prism' Proposal PreSharedKeyID
_PreSharedKeyProposal =
  prism
    PreSharedKeyProposal
    ( \p ->
        case p of
          PreSharedKeyProposal pskp -> Right pskp
          _ -> Left p
    )
{-# INLINE _PreSharedKeyProposal #-}

_ReInitProposal :: Prism' Proposal ReInit
_ReInitProposal =
  prism
    ReInitProposal
    ( \p ->
        case p of
          ReInitProposal rip -> Right rip
          _ -> Left p
    )
{-# INLINE _ReInitProposal #-}

_ExternalInitProposal :: Prism' Proposal ByteString
_ExternalInitProposal =
  prism
    ExternalInitProposal
    ( \p ->
        case p of
          ExternalInitProposal eip -> Right eip
          _ -> Left p
    )
{-# INLINE _ExternalInitProposal #-}

_AppAckProposal :: Prism' Proposal [MessageRange]
_AppAckProposal =
  prism
    AppAckProposal
    ( \p ->
        case p of
          AppAckProposal aap -> Right aap
          _ -> Left p
    )
{-# INLINE _AppAckProposal #-}

_GroupContextExtensionsProposal :: Prism' Proposal [Extension]
_GroupContextExtensionsProposal =
  prism
    GroupContextExtensionsProposal
    ( \p ->
        case p of
          GroupContextExtensionsProposal gcep -> Right gcep
          _ -> Left p
    )
{-# INLINE _GroupContextExtensionsProposal #-}
