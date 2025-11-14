-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Federator.RPC where

import Data.Text qualified as Text
import Imports
import Servant

newtype RPC = RPC Text

instance FromHttpApiData RPC where
  parseUrlPiece :: Text -> Either Text RPC
  parseUrlPiece rpcPath = do
    unless (Text.all isAllowedRPCChar rpcPath) $
      Left "invalid-endpoint"

    when (Text.null rpcPath) $
      Left "invalid-endpoint"
    pure $ RPC rpcPath

isAllowedRPCChar :: Char -> Bool
isAllowedRPCChar c = isAsciiLower c || isAsciiUpper c || isNumber c || c == '_' || c == '-'
