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

module Util.Logging where

import Crypto.Hash (SHA256, hash)
import Data.Handle (Handle (fromHandle))
import Data.Id (TeamId, UserId)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message (Msg)

sha256String :: Text -> Text
sha256String t =
  let digest = hash @ByteString @SHA256 (encodeUtf8 t)
   in cs . show $ digest

logHandle :: Handle -> (Msg -> Msg)
logHandle handl =
  Log.field "handle_sha256" (sha256String . fromHandle $ handl)

logFunction :: Text -> (Msg -> Msg)
logFunction fn = Log.field "fn" fn . Log.field "module" (getModule fn)
  where
    getModule :: Text -> Text
    getModule t = case T.split (== '.') t of
      [] -> ""
      x -> T.intercalate "." (init x)

logUser :: UserId -> (Msg -> Msg)
logUser uid = Log.field "user" (cs @_ @Text . show $ uid)

logTeam :: TeamId -> (Msg -> Msg)
logTeam tid = Log.field "team" (cs @_ @Text . show $ tid)
