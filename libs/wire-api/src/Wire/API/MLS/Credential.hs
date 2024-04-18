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

import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary
import Data.Binary.Get
import Data.Binary.Parser
import Data.Binary.Parser.Char8
import Data.Binary.Put
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as L
import Data.Domain
import Data.Id
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID
import Imports
import Web.HttpApiData
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | An MLS credential.
--
-- https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-5.3-3
data Credential = BasicCredential ByteString | X509Credential [ByteString]
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform Credential

data CredentialTag = BasicCredentialTag | X509CredentialTag
  deriving stock (Enum, Bounded, Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CredentialTag)

instance ParseMLS CredentialTag where
  parseMLS = parseMLSEnum @Word16 "credential type"

instance SerialiseMLS CredentialTag where
  serialiseMLS = serialiseMLSEnum @Word16

instance ParseMLS Credential where
  parseMLS =
    parseMLS >>= \case
      BasicCredentialTag ->
        BasicCredential
          <$> parseMLSBytes @VarInt
      X509CredentialTag ->
        X509Credential
          <$> parseMLSVector @VarInt (parseMLSBytes @VarInt)

instance SerialiseMLS Credential where
  serialiseMLS (BasicCredential i) = do
    serialiseMLS BasicCredentialTag
    serialiseMLSBytes @VarInt i
  serialiseMLS (X509Credential certs) = do
    serialiseMLS X509CredentialTag
    serialiseMLSVector @VarInt (serialiseMLSBytes @VarInt) certs

credentialTag :: Credential -> CredentialTag
credentialTag (BasicCredential _) = BasicCredentialTag
credentialTag (X509Credential _) = X509CredentialTag

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
      <> T.unpack (clientToText c)
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
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

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
    cid <- ClientId <$> hexadecimal
    char '@'
    dom <-
      either fail pure . (mkDomain . T.pack) =<< many' anyChar
    pure $ ClientIdentity dom uid cid

-- format of the x509 client identity: {userid}%21{deviceid}@{host}
parseX509ClientIdentity :: Get ClientIdentity
parseX509ClientIdentity = do
  b64uuid <- getByteString 22
  uidBytes <- either fail pure $ B64URL.decodeUnpadded b64uuid
  uid <- maybe (fail "Invalid UUID") (pure . Id) $ fromByteString (L.fromStrict uidBytes)
  string "%21"
  cid <- ClientId <$> hexadecimal
  char '@'
  dom <-
    either fail pure . (mkDomain . T.pack) =<< many' anyChar
  pure $ ClientIdentity dom uid cid

instance SerialiseMLS ClientIdentity where
  serialiseMLS cid = do
    putByteString $ toASCIIBytes (toUUID (ciUser cid))
    putCharUtf8 ':'
    putStringUtf8 $ T.unpack (clientToText (ciClient cid))
    putCharUtf8 '@'
    putStringUtf8 $ T.unpack (domainText (ciDomain cid))

mkClientIdentity :: Qualified UserId -> ClientId -> ClientIdentity
mkClientIdentity (Qualified uid domain) = ClientIdentity domain uid
