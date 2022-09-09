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

module Jwt.Tools (generateDpopToken) where

import Control.Exception
import Control.Monad.Trans.Except
import Data.ByteString.Conversion (ToByteString, toByteString')
import Data.Id (ClientId (client))
import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Data.PEMKeys
import Data.String.Conversions (cs)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CULong (..), CUShort (..))
import Imports
import Network.HTTP.Types (StdMethod (..))
import Numeric (readHex)
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken

foreign import ccall "generate_dpop_access_token"
  generateDpopTokenFFI ::
    CString ->
    CString ->
    CUShort ->
    CString ->
    CString ->
    CString ->
    CString ->
    CUShort ->
    CULong ->
    CULong ->
    CString ->
    IO CString

foreign import ccall unsafe "free_dpop_access_token" freeDpopAccessToken :: CString -> IO ()

generateDpopToken ::
  (MonadIO m) =>
  Proof ->
  ClientIdentity ->
  Nonce ->
  HttpsUrl ->
  StdMethod ->
  Word16 ->
  Epoch ->
  Epoch ->
  PEMKeys ->
  ExceptT DPoPTokenGenerationError m DPoPAccessToken
generateDpopToken dpopProof cid nonce uri method maxSkewSecs maxExpiration now backendPubkeyBundle = do
  dpopProofCStr <- toCStr dpopProof
  uidCStr <- toCStr $ ciUser cid
  cidCUShort <- case readHex @Word16 (cs $ client $ ciClient cid) of
    [(a, "")] -> pure (CUShort a)
    _ -> throwE InvalidClientId
  domainCStr <- toCStr $ ciDomain cid
  nonceCStr <- toCStr nonce
  uriCStr <- toCStr uri
  methodCStr <- liftIO $ newCString $ cs $ methodToBS method
  backendPubkeyBundleCStr <- toCStr backendPubkeyBundle

  let getToken =
        generateDpopTokenFFI
          dpopProofCStr
          uidCStr
          cidCUShort
          domainCStr
          nonceCStr
          uriCStr
          methodCStr
          (CUShort maxSkewSecs)
          (CULong $ epochNumber maxExpiration)
          (CULong $ epochNumber now)
          backendPubkeyBundleCStr

  let mkAccessToken responseCStr = do
        responseStr <- peekCString responseCStr
        let mbError = readMaybe @Word8 (cs responseStr) >>= mapError
        pure $ maybe (Right $ DPoPAccessToken $ cs responseStr) Left mbError

  ExceptT $ liftIO $ bracket getToken freeDpopAccessToken mkAccessToken
  where
    mapError :: Word8 -> Maybe DPoPTokenGenerationError
    mapError 0 = Nothing
    mapError 1 = Just InvalidDPoPProofSyntax
    mapError 2 = Just InvalidHeaderTyp
    mapError 3 = Just AlgNotSupported
    mapError 4 = Just BadSignature
    mapError _ = error "todo(leif): map other errors"

    toCStr :: forall a m. (ToByteString a, MonadIO m) => a -> m CString
    toCStr = liftIO . newCString . cs . toByteString'

    methodToBS :: StdMethod -> ByteString
    methodToBS = \case
      GET -> "GET"
      POST -> "POST"
      HEAD -> "HEAD"
      PUT -> "PUT"
      DELETE -> "DELETE"
      TRACE -> "TRACE"
      CONNECT -> "CONNECT"
      OPTIONS -> "OPTIONS"
      PATCH -> "PATCH"
