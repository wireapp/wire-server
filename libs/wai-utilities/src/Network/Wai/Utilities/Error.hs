{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wai.Utilities.Error
  ( Error (..),
    ErrorData (..),
    mkError,
    (!>>),
    isBadRequest,
  )
where

import Control.Error
import Data.Aeson hiding (Error)
import Data.Aeson.Types (Pair)
import Data.Domain
import Imports
import Network.HTTP.Types

data Error = Error
  { code :: !Status,
    label :: !LText,
    message :: !LText,
    errorData :: Maybe ErrorData,
    innerError :: Maybe Error
  }
  deriving (Eq, Show, Typeable)

isBadRequest :: Error -> Bool
isBadRequest e = code e == badRequest400

mkError :: Status -> LText -> LText -> Error
mkError c l m = Error c l m Nothing Nothing

instance Exception Error

data ErrorData = FederationErrorData
  { federrDomain :: !Domain,
    federrPath :: !Text
  }
  deriving (Eq, Show, Typeable)

instance ToJSON ErrorData where
  toJSON (FederationErrorData d p) =
    object $
      [ "type" .= ("federation" :: Text),
        "domain" .= d,
        "path" .= p
      ]

instance FromJSON ErrorData where
  parseJSON = withObject "ErrorData" $ \o ->
    FederationErrorData
      <$> o .: "domain"
      <*> o .: "path"

instance ToJSON Error where
  toJSON (Error c l m md inner) =
    object $
      [ "code" .= statusCode c,
        "label" .= l,
        "message" .= m
      ]
        ++ maybe [] dataFields md
        ++ ["inner" .= e | e <- toList inner]
    where
      dataFields :: ErrorData -> [Pair]
      dataFields d = ["data" .= d]

instance FromJSON Error where
  parseJSON = withObject "Error" $ \o ->
    (Error . toEnum <$> (o .: "code"))
      <*> o .: "label"
      <*> o .: "message"
      <*> o .:? "data"
      <*> o .:? "inner"

-- FIXME: This should not live here.
infixl 5 !>>

(!>>) :: (Monad m) => ExceptT a m r -> (a -> b) -> ExceptT b m r
(!>>) = flip fmapLT
