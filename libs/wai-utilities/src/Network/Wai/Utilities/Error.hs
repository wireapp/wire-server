{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wai.Utilities.Error
  ( Error (..),
    ErrorData (..),
    mkError,
    (!>>),
    byteStringError,
  )
where

import Control.Error
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Pair)
import Data.Domain
import Data.Text.Lazy.Encoding (decodeUtf8)
import Imports
import Network.HTTP.Types

data Error = Error
  { code :: !Status,
    label :: !LText,
    message :: !LText,
    errorData :: Maybe ErrorData
  }
  deriving (Show, Typeable)

mkError :: Status -> LText -> LText -> Error
mkError c l m = Error c l m Nothing

instance Exception Error

data ErrorData = FederationErrorData
  { federrDomain :: !Domain,
    federrPath :: !Text
  }
  deriving (Show, Typeable)

instance ToJSON ErrorData where
  toJSON (FederationErrorData d p) =
    object
      [ "type" .= ("federation" :: Text),
        "domain" .= d,
        "path" .= p
      ]

instance FromJSON ErrorData where
  parseJSON = withObject "ErrorData" $ \o ->
    FederationErrorData
      <$> o .: "domain"
      <*> o .: "path"

-- | Assumes UTF-8 encoding.
byteStringError :: Status -> LByteString -> LByteString -> Error
byteStringError s l m = Error s (decodeUtf8 l) (decodeUtf8 m) Nothing

instance ToJSON Error where
  toJSON (Error c l m md) =
    object $
      [ "code" .= statusCode c,
        "label" .= l,
        "message" .= m
      ]
        ++ fromMaybe [] (fmap dataFields md)
    where
      dataFields :: ErrorData -> [Pair]
      dataFields d = ["data" .= d]

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o ->
    Error <$> (toEnum <$> o .: "code")
      <*> o .: "label"
      <*> o .: "message"
      <*> o .:? "data"

-- FIXME: This should not live here.
infixl 5 !>>

(!>>) :: Monad m => ExceptT a m r -> (a -> b) -> ExceptT b m r
(!>>) = flip fmapLT
