{-# LANGUAGE DerivingVia #-}

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

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.ByteString.Conversion (FromByteString (..), toByteString')
import Data.Csv (DefaultOrdered (..), FromNamedRecord (..), Parser, ToNamedRecord (..), namedRecord, (.:))
import Data.Handle (Handle)
import Data.Json.Util (UTCTimeMillis)
import Data.Misc (HttpsUrl)
import Data.Vector (fromList)
import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (GenericUniform))
import Wire.API.Team.Role (Role)
import Wire.API.User (Name)
import Wire.API.User.Identity (Email)
import Wire.API.User.Profile (ManagedBy)

data TeamExportUser = TeamExportUser
  { tExportDisplayName :: Name,
    tExportHandle :: Maybe Handle,
    tExportEmail :: Maybe Email,
    tExportRole :: Maybe Role,
    tExportCreatedOn :: Maybe UTCTimeMillis,
    tExportInvitedBy :: Maybe Handle,
    tExportIdpIssuer :: Maybe HttpsUrl,
    tExportManagedBy :: ManagedBy
    -- FUTUREWORK: @tExportRichProfile :: Maybe RichInfo  -- (rendering: one key-value pair per csv column, sorted alphanumerically by key)@
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform TeamExportUser)

instance ToNamedRecord TeamExportUser where
  toNamedRecord row =
    namedRecord
      [ ("display name", toByteString' (tExportDisplayName row)),
        ("handle", maybe "" toByteString' (tExportHandle row)),
        ("email", maybe "" toByteString' (tExportEmail row)),
        ("role", maybe "" toByteString' (tExportRole row)),
        ("created on", maybe "" toByteString' (tExportCreatedOn row)),
        ("invited by", maybe "" toByteString' (tExportInvitedBy row)),
        ("idp issuer", maybe "" toByteString' (tExportIdpIssuer row)),
        ("managed by", toByteString' (tExportManagedBy row))
      ]

instance DefaultOrdered TeamExportUser where
  headerOrder =
    const $
      fromList
        [ "display name",
          "handle",
          "email",
          "role",
          "created on",
          "invited by",
          "idp issuer",
          "managed by"
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
      <$> (nrec .: "display name" >>= parseByteString)
      <*> (nrec .: "handle" >>= allowEmpty parseByteString)
      <*> (nrec .: "email" >>= allowEmpty parseByteString)
      <*> (nrec .: "role" >>= allowEmpty parseByteString)
      <*> (nrec .: "created on" >>= allowEmpty parseByteString)
      <*> (nrec .: "invited by" >>= allowEmpty parseByteString)
      <*> (nrec .: "idp issuer" >>= allowEmpty parseByteString)
      <*> (nrec .: "managed by" >>= parseByteString)
