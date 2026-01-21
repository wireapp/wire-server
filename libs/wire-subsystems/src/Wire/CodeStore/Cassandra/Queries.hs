-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.CodeStore.Cassandra.Queries where

import Cassandra as C hiding (Value)
import Data.Id
import Imports
import Wire.API.Conversation.Code
import Wire.API.Password (Password)

insertCode :: PrepQuery W (Key, Value, ConvId, Maybe Password, Int32) ()
insertCode = "INSERT INTO conversation_codes (key, value, conversation, scope, password) VALUES (?, ?, ?, 1, ?) USING TTL ?"

lookupCode :: PrepQuery R (Identity Key) (Value, Int32, ConvId, Maybe Password)
lookupCode = "SELECT value, ttl(value), conversation, password FROM conversation_codes WHERE key = ? AND scope = 1"

deleteCode :: PrepQuery W (Identity Key) ()
deleteCode = "DELETE FROM conversation_codes WHERE key = ? AND scope = 1"

selectAllCodes :: PrepQuery R () (Key, Value, Int32, ConvId, Maybe Password)
selectAllCodes = "SELECT key, value, ttl(value), conversation, password FROM conversation_codes"
