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

module Data.ZAuth.Validation
  ( Failure (..),
    check,
  )
where

import Control.Error (runExceptT, throwE)
import Data.ByteString.Conversion
import Data.Time.Clock.POSIX
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vec
import Data.ZAuth.Token
import Imports
import Sodium.Crypto.Sign (PublicKey, verifyWith)

data Failure
  = -- | The token signature is incorrect.
    Falsified
  | -- | The token is expired.
    Expired
  | -- | Invalid token.
    Invalid
  | -- | This operation is unsupported on this token type
    Unsupported
  deriving (Eq, Show)

instance Exception Failure

check :: (SerializableToken t, MonadIO m) => Vector PublicKey -> Token t -> m (Either Failure ())
check pubKeys t = runExceptT $ do
  let dat = toByteString' $ writeData t.header t.body
  let k = t.header.key
  when (k < 1 || k > Vec.length pubKeys) $
    throwE Invalid
  ok <- liftIO $ verifyWith (pubKeys ! (k - 1)) t.signature dat
  unless ok $
    throwE Falsified
  isExpired <-
    if t.header.time == -1
      then pure False
      else (t.header.time <) <$> now
  when isExpired $
    throwE Expired

now :: (MonadIO m) => m Integer
now = floor <$> liftIO getPOSIXTime
