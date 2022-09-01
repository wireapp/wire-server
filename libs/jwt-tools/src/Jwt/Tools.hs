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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Jwt.Tools (testFfi, generateDpopToken) where

import Data.ByteString.Unsafe
import Data.Coerce
import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Imports
import Network.HTTP.Types (StdMethod)
import Test.QuickCheck
import Wire.API.MLS.Credential (ClientIdentity)
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken (DPoPAccessToken, DPoPTokenGenerationError (..))
import Wire.Arbitrary (Arbitrary (arbitrary))

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

testFfi :: IO ()
testFfi = do
  now <- generate arbitrary
  cstr <- generateDpopTokenFFI (CULong $ epochNumber now)
  str <- peekCString cstr
  putStrLn str
  pure ()

foreign import ccall "generate_dpop_token" generateDpopTokenFFI :: CULong -> IO CString
