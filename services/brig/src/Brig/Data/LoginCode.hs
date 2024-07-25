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

-- | Persistent storage for login codes.
-- TODO: Use Brig.Data.Codes
-- TODO: Move to Brig.User.Auth.DB.LoginCode
module Brig.Data.LoginCode
  ( LoginCode,
    verifyLoginCode,
    lookupLoginCode,
  )
where

import Brig.App (Env, currentTime)
import Cassandra
import Control.Lens (view)
import Data.Code
import Data.Id
import Data.Time.Clock
import Imports
import Wire.API.User.Auth

-- | Timeout of individual codes.
ttl :: NominalDiffTime
ttl = 600

verifyLoginCode :: (MonadClient m, MonadReader Env m) => UserId -> LoginCode -> m Bool
verifyLoginCode u c = do
  code <- retry x1 (query1 codeSelect (params LocalQuorum (Identity u)))
  now <- liftIO =<< view currentTime
  case code of
    Just (c', _, t) | c == c' && t >= now -> deleteLoginCode u >> pure True
    Just (c', n, t) | n > 1 && t > now -> insertLoginCode u c' (n - 1) t >> pure False
    Just (_, _, _) -> deleteLoginCode u >> pure False
    Nothing -> pure False

lookupLoginCode :: (MonadReader Env m, MonadClient m) => UserId -> m (Maybe PendingLoginCode)
lookupLoginCode u = do
  now <- liftIO =<< view currentTime
  validate now =<< retry x1 (query1 codeSelect (params LocalQuorum (Identity u)))
  where
    validate now (Just (c, _, t)) | now < t = pure (Just (pending c now t))
    validate _ _ = pure Nothing
    pending c now t = PendingLoginCode c (timeout now t)
    timeout now t = Timeout (t `diffUTCTime` now)

deleteLoginCode :: (MonadClient m) => UserId -> m ()
deleteLoginCode u = retry x5 . write codeDelete $ params LocalQuorum (Identity u)

insertLoginCode :: (MonadClient m) => UserId -> LoginCode -> Int32 -> UTCTime -> m ()
insertLoginCode u c n t = retry x5 . write codeInsert $ params LocalQuorum (u, c, n, t, round ttl)

-- Queries

codeInsert :: PrepQuery W (UserId, LoginCode, Int32, UTCTime, Int32) ()
codeInsert = "INSERT INTO login_codes (user, code, retries, timeout) VALUES (?, ?, ?, ?) USING TTL ?"

codeSelect :: PrepQuery R (Identity UserId) (LoginCode, Int32, UTCTime)
codeSelect = "SELECT code, retries, timeout FROM login_codes WHERE user = ?"

codeDelete :: PrepQuery W (Identity UserId) ()
codeDelete = "DELETE FROM login_codes WHERE user = ?"
