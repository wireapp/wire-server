{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

-- | Email job payload types (WPB-27255).
--
-- Email delivery is queued to the background-worker as a 'BackgroundJobSendEmail'
-- job. The actual SMTP/SES send happens in the worker; brig only enqueues.
--
-- @wire-api@ cannot depend on @mime-mail@, so a mail is serialised as the plain
-- records below. The @Mail@ <-> record conversion lives in @wire-subsystems@
-- ("Wire.EmailSending.Queueing"). Flat part content is stored as base64-encoded
-- 'Text'; nested alternative groups are modelled recursively via
-- 'SerializablePartContent'.
module Wire.API.BackgroundJobs.Email where

import Control.Arrow ((&&&))
import Control.Lens (makePrisms)
import Data.Aeson qualified as Aeson
import Data.Schema
import Imports
import Test.QuickCheck qualified as QC
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))

data SerializableMailAddress = SerializableMailAddress
  { smaName :: !(Maybe Text),
    smaEmail :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SerializableMailAddress)
  deriving (Arbitrary) via GenericUniform SerializableMailAddress

instance ToSchema SerializableMailAddress where
  schema =
    object $
      SerializableMailAddress
        <$> (.smaName) .= maybe_ (optField "name" schema)
        <*> (.smaEmail) .= field "email" schema

data SerializableMailHeader = SerializableMailHeader
  { smhName :: !Text,
    smhValue :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SerializableMailHeader)
  deriving (Arbitrary) via GenericUniform SerializableMailHeader

instance ToSchema SerializableMailHeader where
  schema =
    object $
      SerializableMailHeader
        <$> (.smhName) .= field "name" schema
        <*> (.smhValue) .= field "value" schema

data SerializableEncoding
  = SerializableEncodingNone
  | SerializableEncodingBase64
  | SerializableEncodingQuotedPrintableBinary
  | SerializableEncodingQuotedPrintableText
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SerializableEncoding)
  deriving (Arbitrary) via GenericUniform SerializableEncoding

instance ToSchema SerializableEncoding where
  schema =
    enum @Text $
      mconcat
        [ element "none" SerializableEncodingNone,
          element "base64" SerializableEncodingBase64,
          element "quoted-printable-binary" SerializableEncodingQuotedPrintableBinary,
          element "quoted-printable-text" SerializableEncodingQuotedPrintableText
        ]

data SerializableDisposition = SerializableDisposition
  { smdType :: !SerializableDispositionType,
    smdFilename :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SerializableDisposition)
  deriving (Arbitrary) via GenericUniform SerializableDisposition

instance ToSchema SerializableDisposition where
  schema =
    object $
      SerializableDisposition
        <$> (.smdType) .= field "type" schema
        <*> (.smdFilename) .= field "filename" schema

data SerializableDispositionType
  = SerializableDispositionDefault
  | SerializableDispositionInline
  | SerializableDispositionAttachment
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SerializableDispositionType)
  deriving (Arbitrary) via GenericUniform SerializableDispositionType

instance ToSchema SerializableDispositionType where
  schema =
    enum @Text $
      mconcat
        [ element "default" SerializableDispositionDefault,
          element "inline" SerializableDispositionInline,
          element "attachment" SerializableDispositionAttachment
        ]

-- | Mutually recursive with 'SerializableMailPart': a part's content is either
-- flat bytes or a nested alternative group of parts. Both types must be
-- declared before the @makePrisms@ splice below, so 'SerializableMailPart' gets
-- its schema/arbitrary instances after it.
data SerializableMailPart = SerializableMailPart
  { smpType :: !Text,
    smpEncoding :: !SerializableEncoding,
    smpDisposition :: !SerializableDisposition,
    smpHeaders :: ![SerializableMailHeader],
    smpContent :: !SerializablePartContent
  }
  deriving stock (Eq, Show, Generic)

data SerializablePartContent
  = -- | base64-encoded part content
    SerializablePartContentText !Text
  | SerializablePartContentNestedParts ![SerializableMailPart]
  deriving stock (Eq, Show, Generic)

data SerializablePartContentTag
  = SerializablePartContentTextTag
  | SerializablePartContentNestedPartsTag
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via GenericUniform SerializablePartContentTag

instance ToSchema SerializablePartContentTag where
  schema =
    enum @Text $
      mconcat
        [ element "text" SerializablePartContentTextTag,
          element "nested-parts" SerializablePartContentNestedPartsTag
        ]

serializablePartContentTag :: SerializablePartContent -> SerializablePartContentTag
serializablePartContentTag =
  \case
    SerializablePartContentText {} -> SerializablePartContentTextTag
    SerializablePartContentNestedParts {} -> SerializablePartContentNestedPartsTag

makePrisms ''SerializablePartContent

-- NB: this schema is recursive (nested parts reference 'SerializableMailPart',
-- whose schema references this one), so it must never be inlined into an
-- OpenApi document (the bridge inlines properties and would loop); it exists
-- for the Aeson derivation only.
instance ToSchema SerializablePartContent where
  schema = object serializablePartContentObjectSchema

serializablePartContentObjectSchema :: ObjectSchema SwaggerDoc SerializablePartContent
serializablePartContentObjectSchema =
  snd
    <$> (serializablePartContentTag &&& id)
      .= bind
        (fst .= field "type" schema)
        (snd .= dispatch serializablePartContentDataSchema)
  where
    serializablePartContentDataSchema :: SerializablePartContentTag -> ObjectSchema SwaggerDoc SerializablePartContent
    serializablePartContentDataSchema = \case
      SerializablePartContentTextTag ->
        tag _SerializablePartContentText (field "content" schema)
      SerializablePartContentNestedPartsTag ->
        tag _SerializablePartContentNestedParts (field "content" (array schema))

deriving via (Schema SerializablePartContent) instance Aeson.ToJSON SerializablePartContent

deriving via (Schema SerializablePartContent) instance Aeson.FromJSON SerializablePartContent

instance Arbitrary SerializablePartContent where
  arbitrary =
    QC.sized $ \n ->
      if n <= 0
        then SerializablePartContentText <$> arbitrary
        else
          QC.oneof
            [ SerializablePartContentText <$> arbitrary,
              SerializablePartContentNestedParts . getGenericUniform <$> QC.resize (n `div` 4) arbitrary
            ]
  shrink = QC.genericShrink

deriving via (Schema SerializableMailPart) instance Aeson.ToJSON SerializableMailPart

deriving via (Schema SerializableMailPart) instance Aeson.FromJSON SerializableMailPart

deriving via GenericUniform SerializableMailPart instance Arbitrary SerializableMailPart

instance ToSchema SerializableMailPart where
  schema =
    object $
      SerializableMailPart
        <$> (.smpType) .= field "type" schema
        <*> (.smpEncoding) .= field "encoding" schema
        <*> (.smpDisposition) .= field "disposition" schema
        <*> (.smpHeaders) .= field "headers" (array schema)
        <*> (.smpContent) .= field "content" schema

data SerializableMail = SerializableMail
  { smFrom :: !SerializableMailAddress,
    smTo :: ![SerializableMailAddress],
    smCc :: ![SerializableMailAddress],
    smBcc :: ![SerializableMailAddress],
    smHeaders :: ![SerializableMailHeader],
    smParts :: ![[SerializableMailPart]]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SerializableMail)
  deriving (Arbitrary) via GenericUniform SerializableMail

instance ToSchema SerializableMail where
  schema =
    object $
      SerializableMail
        <$> (.smFrom) .= field "from" schema
        <*> (.smTo) .= field "to" (array schema)
        <*> (.smCc) .= field "cc" (array schema)
        <*> (.smBcc) .= field "bcc" (array schema)
        <*> (.smHeaders) .= field "headers" (array schema)
        <*> (.smParts) .= field "parts" (array (array schema))

data SendEmailJob = SendEmailJob
  { sejMail :: !SerializableMail
  }
  deriving stock (Eq, Show, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Schema SendEmailJob)
  deriving (Arbitrary) via GenericUniform SendEmailJob

instance ToSchema SendEmailJob where
  schema =
    object $
      SendEmailJob
        <$> (.sejMail) .= field "mail" schema
