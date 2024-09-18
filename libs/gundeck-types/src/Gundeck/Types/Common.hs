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

module Gundeck.Types.Common where

import Data.Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Char8 qualified as Bytes
import Data.ByteString.Conversion
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Imports
import Network.URI qualified as Net
import Servant.API (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

newtype CannonId = CannonId
  { cannonId :: Text
  }
  deriving
    ( Eq,
      Ord,
      Show,
      FromJSON,
      ToJSON,
      FromByteString,
      ToByteString
    )

instance FromHttpApiData CannonId where
  parseUrlPiece = pure . CannonId

newtype URI = URI
  { fromURI :: Net.URI
  }
  deriving (Eq, Ord, Show)

instance FromJSON URI where
  parseJSON = withText "URI" (parse . Text.unpack)

instance ToJSON URI where
  toJSON uri = String $ Text.pack (show (fromURI uri))

instance ToByteString URI where
  builder = builder . show . fromURI

instance FromByteString URI where
  parser = takeByteString >>= parse . Bytes.unpack

instance ToHttpApiData URI where
  toUrlPiece = decodeUtf8 . toByteString'

parse :: (MonadFail m) => String -> m URI
parse = maybe (fail "Invalid URI") (pure . URI) . Net.parseURI
