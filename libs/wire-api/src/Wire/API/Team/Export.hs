-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Team.Export (TeamExportUser (..)) where

import qualified Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Conversion (FromByteString (..), toByteString')
import Data.Csv (DefaultOrdered (..), FromNamedRecord (..), Parser, ToNamedRecord (..), namedRecord, (.:))
import Data.Handle (Handle)
import Data.Json.Util (UTCTimeMillis)
import Data.Misc (HttpsUrl)
import Data.String.Conversions (cs)
import Data.Vector (fromList)
import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (GenericUniform))
import Wire.API.Team.Role (Role)
import Wire.API.User (Name)
import Wire.API.User.Identity (Email)
import Wire.API.User.Profile (ManagedBy)
import Wire.API.User.RichInfo (RichInfo)

data TeamExportUser = TeamExportUser
  { tExportDisplayName :: Name,
    tExportHandle :: Maybe Handle,
    tExportEmail :: Maybe Email,
    tExportRole :: Maybe Role,
    tExportCreatedOn :: Maybe UTCTimeMillis,
    tExportInvitedBy :: Maybe Handle,
    tExportIdpIssuer :: Maybe HttpsUrl,
    tExportManagedBy :: ManagedBy,
    tExportSAMLNamedId :: Text, -- If SAML IdP and SCIM peer are set up correctly, 'tExportSAMLNamedId' and 'tExportSCIMExternalId' always align.
    tExportSCIMExternalId :: Text,
    tExportSCIMRichInfo :: Maybe RichInfo
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform TeamExportUser)

instance ToNamedRecord TeamExportUser where
  toNamedRecord row =
    namedRecord
      [ ("display_name", toByteString' (tExportDisplayName row)),
        ("handle", maybe "" toByteString' (tExportHandle row)),
        ("email", maybe "" toByteString' (tExportEmail row)),
        ("role", maybe "" toByteString' (tExportRole row)),
        ("created_on", maybe "" toByteString' (tExportCreatedOn row)),
        ("invited_by", maybe "" toByteString' (tExportInvitedBy row)),
        ("idp_issuer", maybe "" toByteString' (tExportIdpIssuer row)),
        ("managed_by", toByteString' (tExportManagedBy row)),
        ("saml_name_id", toByteString' (tExportSAMLNamedId row)),
        ("scim_external_id", toByteString' (tExportSCIMExternalId row)),
        ("scim_rich_info", maybe "" (cs . Aeson.encode) (tExportSCIMRichInfo row))
      ]

instance DefaultOrdered TeamExportUser where
  headerOrder =
    const $
      fromList
        [ "display_name",
          "handle",
          "email",
          "role",
          "created_on",
          "invited_by",
          "idp_issuer",
          "managed_by",
          "saml_name_id",
          "scim_external_id",
          "scim_rich_info"
        ]

allowEmpty :: (ByteString -> Parser a) -> ByteString -> Parser (Maybe a)
allowEmpty _ "" = pure Nothing
allowEmpty p str = Just <$> p str

parseByteString :: forall a. FromByteString a => ByteString -> Parser a
parseByteString bstr =
  case parseOnly (parser @a) bstr of
    Left err -> fail err
    Right thing -> pure thing

instance FromNamedRecord TeamExportUser where
  parseNamedRecord nrec =
    TeamExportUser
      <$> (nrec .: "display_name" >>= parseByteString)
      <*> (nrec .: "handle" >>= allowEmpty parseByteString)
      <*> (nrec .: "email" >>= allowEmpty parseByteString)
      <*> (nrec .: "role" >>= allowEmpty parseByteString)
      <*> (nrec .: "created_on" >>= allowEmpty parseByteString)
      <*> (nrec .: "invited_by" >>= allowEmpty parseByteString)
      <*> (nrec .: "idp_issuer" >>= allowEmpty parseByteString)
      <*> (nrec .: "managed_by" >>= parseByteString)
      <*> (nrec .: "saml_name_id" >>= parseByteString)
      <*> (nrec .: "scim_external_id" >>= parseByteString)
      <*> (nrec .: "scim_rich_info" >>= allowEmpty (maybe (fail "failed to decode RichInfo") pure . Aeson.decode . cs))
