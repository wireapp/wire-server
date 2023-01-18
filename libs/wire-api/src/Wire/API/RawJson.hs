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

module Wire.API.RawJson where

import Data.Aeson as A hiding ((.=))
import Data.Schema
import qualified Data.Swagger as S
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE
import Imports
import Servant
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- | Wrap json content as plain 'LByteString'
-- This type is intented to be used to receive json content as 'LByteString'.
-- Warning: There is no validation of the json content. It may be any string.
newtype RawJson = RawJson {rawJsonBytes :: LByteString}
  deriving (Eq, Show)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema RawJson
  deriving newtype (Arbitrary)

instance {-# OVERLAPPING #-} MimeUnrender JSON RawJson where
  mimeUnrender _ = pure . RawJson

-- TODO: unit test against MimeUnrender
instance ToSchema RawJson where
  schema :: ValueSchema NamedSwaggerDoc RawJson
  schema = textFromRawJson .= fmap rawJsonFromText ((text . T.pack) "RawJson")
    where
      textFromRawJson = TL.toStrict . decodeUtf8 . rawJsonBytes
      rawJsonFromText = RawJson . TLE.encodeUtf8 . TL.fromStrict
