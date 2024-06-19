{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
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

-- | A "default" module for types used in Spar, unless there's a better / more specific place
-- for them.
module Wire.API.User.Saml where

import Control.Lens (makeLenses)
import Control.Monad.Except
import Data.Aeson hiding (fieldLabelModifier)
import Data.Aeson.TH hiding (fieldLabelModifier)
import Data.ByteString (toStrict)
import Data.ByteString.Builder qualified as Builder
import Data.Id (UserId)
import Data.OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types (Symbol)
import Imports
import SAML2.WebSSO
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import URI.ByteString
import Web.Cookie
import Wire.API.User.Orphans ()

----------------------------------------------------------------------------
-- Requests and verdicts

type AReqId = ID AuthnRequest

type AssId = ID Assertion

-- | Clients can request different ways of receiving the final 'AccessVerdict' when fetching their
-- 'AuthnRequest'.  Web-based clients want an html page, mobile clients want to set two URIs for the
-- two resp. 'AccessVerdict' constructors.  This format is stored in cassandra under the request id
-- so that the verdict handler can act on it.
data VerdictFormat
  = VerdictFormatWeb
  | VerdictFormatMobile {_formatGrantedURI :: URI, _formatDeniedURI :: URI}
  deriving (Eq, Show, Generic)

makeLenses ''VerdictFormat

deriveJSON deriveJSONOptions ''VerdictFormat

mkVerdictGrantedFormatMobile :: (MonadError String m) => URI -> SetCookie -> UserId -> m URI
mkVerdictGrantedFormatMobile before cky uid =
  parseURI'
    . substituteVar
      "cookie"
      ( decodeUtf8With lenientDecode
          . toStrict
          . Builder.toLazyByteString
          . renderSetCookie
          $ cky
      )
    . substituteVar "userid" (T.pack . show $ uid)
    $ renderURI before

mkVerdictDeniedFormatMobile :: (MonadError String m) => URI -> Text -> m URI
mkVerdictDeniedFormatMobile before lbl =
  parseURI'
    . substituteVar "label" lbl
    $ renderURI before

substituteVar :: Text -> Text -> Text -> Text
substituteVar var val = substituteVar' ("$" <> var) val . substituteVar' ("%24" <> var) val

substituteVar' :: Text -> Text -> Text -> Text
substituteVar' var val = T.intercalate val . T.splitOn var

-- | (seconds)
newtype TTL (tablename :: Symbol) = TTL {fromTTL :: Int32}
  deriving (Eq, Ord, Show, Num)

showTTL :: (KnownSymbol a) => TTL a -> String
showTTL (TTL i :: TTL a) = "TTL:" <> symbolVal (Proxy @a) <> ":" <> show i

instance FromJSON (TTL a) where
  parseJSON = withScientific "TTL value (seconds)" (pure . TTL . round)

data TTLError = TTLTooLong String String | TTLNegative String
  deriving (Eq, Show)

ttlToNominalDiffTime :: TTL a -> NominalDiffTime
ttlToNominalDiffTime (TTL i32) = fromIntegral i32

data SsoSettings = SsoSettings
  { defaultSsoCode :: !(Maybe IdPId)
  }
  deriving (Generic, Show)

instance FromJSON SsoSettings where
  parseJSON = withObject "SsoSettings" $ \obj -> do
    -- key needs to be present, but can be null
    SsoSettings <$> obj .: "default_sso_code"

instance ToJSON SsoSettings where
  toJSON SsoSettings {defaultSsoCode} =
    object ["default_sso_code" .= defaultSsoCode]

-- Swagger instances

instance ToSchema SsoSettings where
  declareNamedSchema =
    genericDeclareNamedSchema
      defaultSchemaOptions
        { fieldLabelModifier = \case
            "defaultSsoCode" -> "default_sso_code"
            other -> other
        }
