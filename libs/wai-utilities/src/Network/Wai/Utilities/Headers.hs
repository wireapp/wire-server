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

module Network.Wai.Utilities.Headers where

import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), fromByteString', toByteString')
import Data.String.Conversions (cs)
import Data.Swagger.ParamSchema (ToParamSchema (..))
import Data.Text as T
import Imports
import Servant (FromHttpApiData (..), Proxy (Proxy), ToHttpApiData (..))

data CacheControl = NoStore
  deriving (Eq, Show, Generic)

instance ToByteString CacheControl where
  builder NoStore = "no-store"

instance FromByteString CacheControl where
  parser = do
    t :: Text <- parser
    case t & T.toLower of
      "no-store" -> pure NoStore
      _ -> fail $ "Invalid CacheControl type: " ++ show t

instance ToHttpApiData CacheControl where
  toQueryParam = cs . toByteString'

instance FromHttpApiData CacheControl where
  parseQueryParam = maybe (Left "Invalid CacheControl") Right . fromByteString' . cs

instance ToParamSchema CacheControl where
  toParamSchema _ = toParamSchema (Proxy @Text)
