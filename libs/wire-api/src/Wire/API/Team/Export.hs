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

module Wire.API.Team.Export (TeamExportUser (..), quoted, unquoted) where

import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Conversion (FromByteString (..), ToByteString, toByteString')
import Data.Csv (DefaultOrdered (..), FromNamedRecord (..), Parser, ToNamedRecord (..), namedRecord, (.:))
import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Json.Util (UTCTimeMillis, utcTimeSchema)
import Data.Misc (HttpsUrl)
import Data.OpenApi qualified as OpenApi
import Data.Schema
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Data.Time.Format
import Data.Vector (fromList)
import Imports
import Test.QuickCheck
import Wire.API.Team.Role (Role)
import Wire.API.User (AccountStatus (..), Name)
import Wire.API.User.Identity (EmailAddress)
import Wire.API.User.Profile (ManagedBy)
import Wire.API.User.RichInfo (RichInfo)
import Wire.Arbitrary

timestampFormat :: String
timestampFormat = "%Y-%m-%d"

data TeamExportUser = TeamExportUser
  { tExportDisplayName :: Name,
    tExportHandle :: Maybe Handle,
    tExportEmail :: Maybe EmailAddress,
    tExportRole :: Maybe Role,
    tExportCreatedOn :: Maybe UTCTimeMillis,
    tExportInvitedBy :: Maybe Handle,
    tExportIdpIssuer :: Maybe HttpsUrl,
    tExportManagedBy :: ManagedBy,
    tExportSAMLNamedId :: Text, -- If SAML IdP and SCIM peer are set up correctly, 'tExportSAMLNamedId' and 'tExportSCIMExternalId' always align.
    tExportSCIMExternalId :: Text,
    tExportSCIMRichInfo :: Maybe RichInfo,
    tExportUserId :: UserId,
    tExportNumDevices :: Int,
    tExportLastActive :: Maybe UTCTime,
    tExportStatus :: Maybe AccountStatus
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform TeamExportUser)
  deriving (A.ToJSON, A.FromJSON, OpenApi.ToSchema) via (Schema TeamExportUser)

instance ToSchema TeamExportUser where
  schema =
    object "TeamExportUser" $
      TeamExportUser
        <$> tExportDisplayName .= field "display_name" schema
        <*> tExportHandle .= maybe_ (optField "handle" schema)
        <*> tExportEmail .= maybe_ (optField "email" schema)
        <*> tExportRole .= maybe_ (optField "role" schema)
        <*> tExportCreatedOn .= maybe_ (optField "created_on" schema)
        <*> tExportInvitedBy .= maybe_ (optField "invited_by" schema)
        <*> tExportIdpIssuer .= maybe_ (optField "idp_issuer" schema)
        <*> tExportManagedBy .= field "managed_by" schema
        <*> tExportSAMLNamedId .= field "saml_name_id" schema
        <*> tExportSCIMExternalId .= field "scim_external_id" schema
        <*> tExportSCIMRichInfo .= maybe_ (optField "scim_rich_info" schema)
        <*> tExportUserId .= field "user_id" schema
        <*> tExportNumDevices .= field "num_devices" schema
        <*> tExportLastActive .= maybe_ (optField "last_active" utcTimeSchema)
        <*> tExportStatus .= maybe_ (optField "status" schema)

instance ToNamedRecord TeamExportUser where
  toNamedRecord row =
    namedRecord
      [ ("display_name", secureCsvFieldToByteString (tExportDisplayName row)),
        ("handle", maybe "" secureCsvFieldToByteString (tExportHandle row)),
        ("email", maybe "" secureCsvFieldToByteString (tExportEmail row)),
        ("role", maybe "" secureCsvFieldToByteString (tExportRole row)),
        ("created_on", maybe "" secureCsvFieldToByteString (tExportCreatedOn row)),
        ("invited_by", maybe "" secureCsvFieldToByteString (tExportInvitedBy row)),
        ("idp_issuer", maybe "" secureCsvFieldToByteString (tExportIdpIssuer row)),
        ("managed_by", secureCsvFieldToByteString (tExportManagedBy row)),
        ("saml_name_id", secureCsvFieldToByteString (tExportSAMLNamedId row)),
        ("scim_external_id", secureCsvFieldToByteString (tExportSCIMExternalId row)),
        ("scim_rich_info", maybe "" (C.toStrict . Aeson.encode) (tExportSCIMRichInfo row)),
        ("user_id", secureCsvFieldToByteString (tExportUserId row)),
        ("num_devices", secureCsvFieldToByteString (tExportNumDevices row)),
        ( "last_active",
          C.pack
            ( maybe
                ""
                (formatTime defaultTimeLocale timestampFormat)
                (tExportLastActive row)
            )
        ),
        ("status", maybe "" formatAccountStatus (tExportStatus row))
      ]

secureCsvFieldToByteString :: forall a. (ToByteString a) => a -> ByteString
secureCsvFieldToByteString = quoted . toByteString'

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
          "scim_rich_info",
          "user_id",
          "num_devices",
          "last_active",
          "status"
        ]

allowEmpty :: (ByteString -> Parser a) -> ByteString -> Parser (Maybe a)
allowEmpty _ "" = pure Nothing
allowEmpty p str = Just <$> p str

parseByteString :: forall a. (FromByteString a) => ByteString -> Parser a
parseByteString bstr =
  case parseOnly (parser @a) (C.fromStrict (unquoted bstr)) of
    Left err -> fail err
    Right thing -> pure thing

parseUTCTime :: ByteString -> Parser UTCTime
parseUTCTime b = do
  s <- either (fail . displayException) pure $ T.decodeUtf8' b
  parseTimeM False defaultTimeLocale timestampFormat (T.unpack s)

parseAccountStatus :: ByteString -> Parser AccountStatus
parseAccountStatus "active" = pure Active
parseAccountStatus "suspended" = pure Suspended
parseAccountStatus "deleted" = pure Deleted
parseAccountStatus "ephemeral" = pure Ephemeral
parseAccountStatus "pending-invitation" = pure PendingInvitation
parseAccountStatus _ = fail "invalid account status"

formatAccountStatus :: AccountStatus -> ByteString
formatAccountStatus Active = "active"
formatAccountStatus Suspended = "suspended"
formatAccountStatus Deleted = "deleted"
formatAccountStatus Ephemeral = "ephemeral"
formatAccountStatus PendingInvitation = "pending-invitation"

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
      <*> ( nrec .: "scim_rich_info"
              >>= allowEmpty
                ( maybe (fail "failed to decode RichInfo") pure
                    . Aeson.decode
                    . C.fromStrict
                )
          )
      <*> (nrec .: "user_id" >>= parseByteString)
      <*> (nrec .: "num_devices" >>= parseByteString)
      <*> (nrec .: "last_active" >>= allowEmpty parseUTCTime)
      <*> (nrec .: "status" >>= allowEmpty parseAccountStatus)

quoted :: ByteString -> ByteString
quoted bs = case C.uncons bs of
  -- fields that begin with a disallowed character are prepended with a single quote
  Just ('=', _) -> '\'' `C.cons` bs
  Just ('+', _) -> '\'' `C.cons` bs
  Just ('-', _) -> '\'' `C.cons` bs
  Just ('@', _) -> '\'' `C.cons` bs
  -- tab
  Just ('\x0009', _) -> '\'' `C.cons` bs
  -- carriage return
  Just ('\x000D', _) -> '\'' `C.cons` bs
  -- if a field begins with a single quote we have to prepend another single quote to be able to decode back correctly
  Just ('\'', _) -> '\'' `C.cons` bs
  -- everything else is fine
  _ -> bs

unquoted :: ByteString -> ByteString
unquoted bstr = case C.uncons bstr of
  Just ('\'', t) -> t
  _ -> bstr
