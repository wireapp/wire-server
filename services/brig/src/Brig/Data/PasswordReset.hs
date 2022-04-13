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

-- | Persistent storage for password reset codes.
-- TODO: Use Brig.Data.Codes
module Brig.Data.PasswordReset
  ( createPasswordResetCode,
    verifyPasswordResetCode,
    lookupPasswordResetCode,
    mkPasswordResetKey,
  )
where

import Brig.Data.Instances ()
import Brig.Sem.BrigTime (BrigTime)
import qualified Brig.Sem.BrigTime as Time
import Brig.Sem.CodeStore
import Brig.Types
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy

maxAttempts :: Int32
maxAttempts = 3

ttl :: NominalDiffTime
ttl = 3600 -- 60 minutes

createPasswordResetCode ::
  Members
    '[ BrigTime,
       CodeStore
     ]
    r =>
  UserId ->
  Either Email Phone ->
  Sem r PasswordResetPair
createPasswordResetCode u target = do
  key <- mkPasswordResetKey u
  now <- Time.get
  code <- either (const generateEmailCode) (const generatePhoneCode) target
  codeInsert key code u maxAttempts (ttl `addUTCTime` now) (round ttl)
  return (key, code)

lookupPasswordResetCode ::
  Members
    '[ BrigTime,
       CodeStore
     ]
    r =>
  UserId ->
  Sem r (Maybe PasswordResetCode)
lookupPasswordResetCode u = do
  key <- mkPasswordResetKey u
  now <- Time.get
  validate now =<< codeSelect key
  where
    validate now (Just (c, _, _, Just t)) | t > now = return $ Just c
    validate _ _ = return Nothing

verifyPasswordResetCode ::
  Members
    '[ BrigTime,
       CodeStore
     ]
    r =>
  PasswordResetPair ->
  Sem r (Maybe UserId)
verifyPasswordResetCode (k, c) = do
  now <- Time.get
  code <- codeSelect k
  case code of
    Just (c', u, _, Just t) | c == c' && t >= now -> return (Just u)
    Just (c', u, Just n, Just t) | n > 1 && t > now -> do
      codeInsert k c' u (n - 1) t (round ttl)
      return Nothing
    Just (_, _, _, _) -> codeDelete k $> Nothing
    Nothing -> return Nothing
