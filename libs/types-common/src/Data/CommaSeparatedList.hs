{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Data.CommaSeparatedList where

import Control.Lens ((?~))
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString.Conversion (FromByteString, List, fromList, parser, runParser)
import Data.Proxy (Proxy (..))
import Data.Range (Bounds, Range)
import Data.Swagger (CollectionFormat (CollectionCSV), SwaggerItems (SwaggerItemsPrimitive), SwaggerType (SwaggerString), ToParamSchema (..), items, type_)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Imports
import Servant (FromHttpApiData (..))

newtype CommaSeparatedList a = CommaSeparatedList {fromCommaSeparatedList :: [a]}
  deriving stock (Show, Eq)
  deriving newtype (Bounds)

instance FromByteString (List a) => FromHttpApiData (CommaSeparatedList a) where
  parseUrlPiece t =
    CommaSeparatedList . fromList <$> Bifunctor.first Text.pack (runParser parser $ encodeUtf8 t)

instance ToParamSchema (CommaSeparatedList a) where
  toParamSchema _ = mempty & type_ ?~ SwaggerString

instance (ToParamSchema a, ToParamSchema (Range n m [a])) => ToParamSchema (Range n m (CommaSeparatedList a)) where
  toParamSchema _ =
    toParamSchema (Proxy @(Range n m [a]))
      & items ?~ SwaggerItemsPrimitive (Just CollectionCSV) (toParamSchema (Proxy @a))
