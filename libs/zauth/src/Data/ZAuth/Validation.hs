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

import Data.ByteString.Conversion
import Data.Time.Clock.POSIX
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vec
import Data.ZAuth.CryptoSign (CryptoSign, verifyWith)
import Data.ZAuth.Token
import Imports
import Polysemy
import Polysemy.Error
import Sodium.Crypto.Sign (PublicKey)
import Wire.Sem.FromUTC (FromUTC (fromUTCTime))
import Wire.Sem.Now qualified as Now

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

check :: (SerializableToken t, Member CryptoSign r, Member Now.Now r) => Vector PublicKey -> Token t -> Sem r (Either Failure ())
check pubKeys t = runError $ do
  let dat = toByteString' $ writeData t.header t.body
  let k = t.header.key
  when (k < 1 || k > Vec.length pubKeys) $
    throw Invalid
  ok <- verifyWith (pubKeys ! (k - 1)) t.signature dat
  unless ok $
    throw Falsified
  isExpired <-
    if t.header.time == -1
      then pure False
      else (t.header.time <) <$> now
  when isExpired $
    throw Expired

now :: (Member Now.Now r) => Sem r Integer
now = floor . fromUTCTime @POSIXTime <$> Now.get
