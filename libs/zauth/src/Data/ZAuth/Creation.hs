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

module Data.ZAuth.Creation
  ( -- * Types
    SigningKey (..),
    TokenExpiry (..),

    -- * Actions
    newToken,
    renewToken,
  )
where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX
import Data.ZAuth.CryptoSign
import Data.ZAuth.Token hiding (signature)
import Imports
import Polysemy
import Sodium.Crypto.Sign (SecretKey, Signature)
import Wire.Sem.FromUTC
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

data TokenExpiry
  = TokenExpiresAfter Integer
  | TokenExpiresAt POSIXTime
  | TokenNeverExpires

data SigningKey = SigningKey
  { keyIdx :: Int,
    key :: SecretKey
  }

renewToken :: forall t r. (SerializableToken t, Member Now r, Member CryptoSign r) => SigningKey -> Integer -> Header t -> Body t -> Sem r (Token t)
renewToken signingKey dur hdr bdy = do
  newToken signingKey (TokenExpiresAfter dur) (hdr.tag) bdy

tokenVersion :: Int
tokenVersion = 1

newToken :: forall t r. (SerializableToken t, Member CryptoSign r, Member Now r) => SigningKey -> TokenExpiry -> Maybe Tag -> Body t -> Sem r (Token t)
newToken signingKey tokenExpiry mTag a = do
  tokenTime <- case tokenExpiry of
    TokenExpiresAt t -> pure t
    TokenNeverExpires -> pure (-1)
    TokenExpiresAfter ttl -> expiry ttl
  let h = Header tokenVersion signingKey.keyIdx (floor tokenTime) mTag
  s <- signToken signingKey.key h a
  pure $ Token s h a

-----------------------------------------------------------------------------
-- Internal

signToken :: (SerializableToken t, Member CryptoSign r) => SecretKey -> Header t -> Body t -> Sem r Signature
signToken key h a =
  sign key . toStrict . toLazyByteString $ writeData h a

expiry :: (Member Now r) => Integer -> Sem r POSIXTime
expiry d = (fromInteger d +) . fromUTCTime <$> Now.get
