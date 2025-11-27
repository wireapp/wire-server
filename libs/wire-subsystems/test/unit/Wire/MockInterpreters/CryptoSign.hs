{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Wire.MockInterpreters.CryptoSign where

import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.CryptoSign qualified as CryptoSign
import Imports
import Polysemy
import Sodium.Crypto.Sign qualified as Sodium
import System.IO.Unsafe (unsafePerformIO)

-- Perhaps it is also possible to implment this with crypton, but EdDSA with
-- Curve_Edwards25519 uses secret key size of 32, while the libsodium has secret
-- key size of 64.
runCryptoSignUnsafe :: InterpreterFor CryptoSign r
runCryptoSignUnsafe = interpret \case
  CryptoSign.Sign sec msg -> do
    pure $ unsafePerformIO $ Sodium.signature sec msg
  CryptoSign.VerifyWith pubKey sig msg -> do
    pure $ unsafePerformIO $ Sodium.verifyWith pubKey sig msg
