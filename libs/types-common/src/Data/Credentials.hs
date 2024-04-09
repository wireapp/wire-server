-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Data.Credentials where

import Data.Aeson (FromJSON)
import Data.ByteString.Base64 qualified as B64
import Data.Text
import Data.Text.Encoding qualified as TE
import Imports
import Network.HTTP.Types.Header

-- | Generic credentials for authenticating a user. Usually used for deserializing from a secret yaml file.
data Credentials = Credentials
  { username :: Text,
    password :: Text
  }
  deriving stock (Generic)

instance FromJSON Credentials

mkBasicAuthHeader :: Credentials -> Header
mkBasicAuthHeader (Credentials u p) = (hAuthorization, "Basic " <> B64.encode (TE.encodeUtf8 (u <> ":" <> p)))
