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

module Brig.RPC.Decode where

import Bilge.Response
import Control.Monad.Catch
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Imports

decodeBody ::
  (Typeable a, FromJSON a, MonadThrow m) =>
  Text ->
  Response (Maybe LBS.ByteString) ->
  m a
decodeBody ctx = responseJsonThrow (ParseException ctx)

-- | Failed to parse a response from another service.
data ParseException = ParseException
  { _parseExceptionRemote :: !Text,
    _parseExceptionMsg :: String
  }

instance Show ParseException where
  show (ParseException r m) =
    "Failed to parse response from remote "
      ++ Text.unpack r
      ++ " with message: "
      ++ m

instance Exception ParseException
