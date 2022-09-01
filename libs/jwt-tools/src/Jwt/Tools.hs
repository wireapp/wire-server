{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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

module Jwt.Tools (testFfi, generateDpopToken) where

import Data.ByteString.Unsafe
import Data.Coerce
import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Imports
import Network.HTTP.Types (StdMethod)
import Wire.API.MLS.Credential (ClientIdentity)
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken (DPoPAccessToken, DPoPTokenGenerationError (..))

generateDpopToken ::
  ByteString ->
  ClientIdentity ->
  Nonce ->
  HttpsUrl ->
  StdMethod ->
  Word16 ->
  Word16 ->
  Epoch ->
  ByteString ->
  IO (Either DPoPTokenGenerationError DPoPAccessToken)
generateDpopToken _dpopProof _cid _nonce _uri _method _maxSkewSecs _maxExpiration _now _backendPubkeyBundle = do
  pure $ Left DPoPTokenGenerationError

data RustByteArray = RustByteArray
  { rbaData :: Ptr Word8,
    rbaLen :: Word64
  }
  deriving (Eq, Show, Generic)

instance Storable RustByteArray where
  alignment = sizeOf
  sizeOf _ = sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: Word64)
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr (sizeOf (undefined :: Ptr Word8))
    pure (RustByteArray a b)
  poke _ptr _ = undefined

testFfi :: IO ()
testFfi = do
  let now = Epoch 5555
  print $ sizeOf (undefined :: RustByteArray)
  allocaBytes 16000 $ \ptr -> do
    generateDpopTokenFFI (CULong $ epochNumber now) ptr
    rustByteArray <- peek ptr
    print rustByteArray
    let cstringlen = (coerce $ rbaData rustByteArray, fromIntegral $ rbaLen rustByteArray)
    unsafePackCStringLen cstringlen >>= print
  pure ()

foreign import ccall "generate_dpop_token" generateDpopTokenFFI :: CULong -> Ptr RustByteArray -> IO ()
