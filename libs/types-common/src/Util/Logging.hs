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

module Util.Logging where

import Crypto.Hash (SHA256, hash)
import Data.Handle (Handle (fromHandle))
import Data.Id (TeamId, UserId)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Imports
import System.Logger.Class qualified as Log
import System.Logger.Message (Msg)
import Text.Email.Parser

sha256String :: Text -> Text
sha256String t =
  let digest = hash @ByteString @SHA256 (encodeUtf8 t)
   in T.pack . show $ digest

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
logUser uid = Log.field "user" (T.pack . show $ uid)

logTeam :: TeamId -> (Msg -> Msg)
logTeam tid = Log.field "team" (T.pack . show $ tid)

logEmail :: EmailAddress -> (Msg -> Msg)
logEmail email =
  Log.field "email_sha256" (sha256String . T.pack . show $ email)
