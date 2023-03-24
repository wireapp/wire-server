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

module Wire.API.MLS.Credential where

import Control.Error.Util
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor
import Data.Binary
import Data.Binary.Get
import Data.Binary.Parser
import Data.Binary.Parser.Char8
import Data.Binary.Put
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID
import GHC.Records
import Imports
import Web.HttpApiData
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | An MLS credential.
--
-- Only the @BasicCredential@ type is supported.
data Credential = BasicCredential ByteString
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Credential

data CredentialTag where
  BasicCredentialTag :: CredentialTag
  deriving stock (Enum, Bounded, Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CredentialTag)

instance ParseMLS CredentialTag where
  parseMLS = parseMLSEnum @Word16 "credential type"

instance ParseMLS Credential where
  parseMLS =
    parseMLS >>= \case
      BasicCredentialTag ->
        BasicCredential
          <$> parseMLSBytes @VarInt

credentialTag :: Credential -> CredentialTag
credentialTag BasicCredential {} = BasicCredentialTag

instance HasField "identityData" Credential ByteString where
  getField (BasicCredential i) = i

data ClientIdentity = ClientIdentity
  { ciDomain :: Domain,
    ciUser :: UserId,
    ciClient :: ClientId
  }
  deriving stock (Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientIdentity
  deriving (Arbitrary) via (GenericUniform ClientIdentity)

instance Show ClientIdentity where
  show (ClientIdentity dom u c) =
    show u
      <> ":"
      <> T.unpack (client c)
      <> "@"
      <> T.unpack (domainText dom)

cidQualifiedClient :: ClientIdentity -> Qualified (UserId, ClientId)
cidQualifiedClient cid = Qualified (ciUser cid, ciClient cid) (ciDomain cid)

cidQualifiedUser :: ClientIdentity -> Qualified UserId
cidQualifiedUser = fmap fst . cidQualifiedClient

instance ToSchema ClientIdentity where
  schema =
    object "ClientIdentity" $
      ClientIdentity
        <$> ciDomain .= field "domain" schema
        <*> ciUser .= field "user_id" schema
        <*> ciClient .= field "client_id" schema

instance S.ToParamSchema ClientIdentity where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

instance FromHttpApiData ClientIdentity where
  parseHeader = decodeMLS'
  parseUrlPiece = decodeMLS' . T.encodeUtf8

instance ToHttpApiData ClientIdentity where
  toHeader = encodeMLS'
  toUrlPiece = T.decodeUtf8 . encodeMLS'

instance ParseMLS ClientIdentity where
  parseMLS = do
    uid <-
      maybe (fail "Invalid UUID") (pure . Id) . fromASCIIBytes =<< getByteString 36
    char ':'
    cid <- newClientId <$> hexadecimal
    char '@'
    dom <-
      either fail pure . (mkDomain . T.pack) =<< many' anyChar
    pure $ ClientIdentity dom uid cid

instance SerialiseMLS ClientIdentity where
  serialiseMLS cid = do
    putByteString $ toASCIIBytes (toUUID (ciUser cid))
    putCharUtf8 ':'
    putStringUtf8 $ T.unpack (client (ciClient cid))
    putCharUtf8 '@'
    putStringUtf8 $ T.unpack (domainText (ciDomain cid))

mkClientIdentity :: Qualified UserId -> ClientId -> ClientIdentity
mkClientIdentity (Qualified uid domain) = ClientIdentity domain uid

-- | Possible uses of a private key in the context of MLS.
data SignaturePurpose
  = -- | Creating external remove proposals.
    RemovalPurpose
  deriving (Eq, Ord, Show, Bounded, Enum)

signaturePurposeName :: SignaturePurpose -> Text
signaturePurposeName RemovalPurpose = "removal"

signaturePurposeFromName :: Text -> Either String SignaturePurpose
signaturePurposeFromName name =
  note ("Unsupported signature purpose " <> T.unpack name)
    . getAlt
    $ flip foldMap [minBound .. maxBound]
    $ \s ->
      guard (signaturePurposeName s == name) $> s

instance FromJSON SignaturePurpose where
  parseJSON =
    Aeson.withText "SignaturePurpose" $
      either fail pure . signaturePurposeFromName

instance FromJSONKey SignaturePurpose where
  fromJSONKey =
    Aeson.FromJSONKeyTextParser $
      either fail pure . signaturePurposeFromName

instance S.ToParamSchema SignaturePurpose where
  toParamSchema _ = mempty & S.type_ ?~ S.SwaggerString

instance FromHttpApiData SignaturePurpose where
  parseQueryParam = first T.pack . signaturePurposeFromName

instance ToJSON SignaturePurpose where
  toJSON = Aeson.String . signaturePurposeName

instance ToJSONKey SignaturePurpose where
  toJSONKey = Aeson.toJSONKeyText signaturePurposeName
