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
import Foreign.C (CUChar (..))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Imports
import Network.HTTP.Types (StdMethod (..))
import Numeric (readHex)
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken

data JwtResponse

foreign import ccall unsafe "generate_dpop_access_token"
  generate_dpop_access_token ::
    CString ->
    CString ->
    Word16 ->
    CString ->
    CString ->
    CString ->
    CString ->
    Word16 ->
    Word64 ->
    Word64 ->
    CString ->
    IO (Ptr JwtResponse)

foreign import ccall unsafe "free_dpop_access_token" free_dpop_access_token :: Ptr JwtResponse -> IO ()

foreign import ccall unsafe "get_error" get_error :: Ptr JwtResponse -> Ptr CUChar

foreign import ccall unsafe "get_token" get_token :: Ptr JwtResponse -> CString

createToken ::
  CString ->
  CString ->
  Word16 ->
  CString ->
  CString ->
  CString ->
  CString ->
  Word16 ->
  Word64 ->
  Word64 ->
  CString ->
  IO (Maybe (Ptr JwtResponse))
createToken dpopProof user client domain nonce uri method maxSkewSecs expiration now backendKeys = do
  ptr <- generate_dpop_access_token dpopProof user client domain nonce uri method maxSkewSecs expiration now backendKeys
  if ptr /= nullPtr
    then pure $ Just ptr
    else pure Nothing

getError :: Ptr JwtResponse -> IO (Maybe Word8)
getError ptr = do
  let errorPtr = get_error ptr
  if errorPtr /= nullPtr
    then Just . fromIntegral <$> peek errorPtr
    else pure Nothing

getToken :: Ptr JwtResponse -> IO (Maybe String)
getToken ptr = do
  let tokenPtr = get_token ptr
  if tokenPtr /= nullPtr
    then Just <$> peekCString tokenPtr
    else pure Nothing

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
    [(a, "")] -> pure a
    _ -> throwE ClientIdSyntaxError
  domainCStr <- toCStr $ ciDomain cid
  nonceCStr <- toCStr nonce
  uriCStr <- toCStr uri
  methodCStr <- liftIO $ newCString $ cs $ methodToBS method
  backendPubkeyBundleCStr <- toCStr backendPubkeyBundle

  let before =
        createToken
          dpopProofCStr
          uidCStr
          cidCUShort
          domainCStr
          nonceCStr
          uriCStr
          methodCStr
          maxSkewSecs
          (epochNumber maxExpiration)
          (epochNumber now)
          backendPubkeyBundleCStr

  let mkAccessToken response = do
        case response of
          Nothing -> pure $ Left FfiError
          Just r -> do
            mErr <- getError r
            print mErr
            mToken <- getToken r
            pure $ toResult mErr mToken

  let free = maybe (pure ()) free_dpop_access_token

  ExceptT $ liftIO $ bracket before free mkAccessToken
  where
    toResult :: Maybe Word8 -> Maybe String -> Either DPoPTokenGenerationError DPoPAccessToken
    -- the only valid case is when the error=0 (meaning no error) and the token is not null
    toResult Nothing (Just token) = Right $ DPoPAccessToken $ cs token
    -- error=1 corresponds to an unknown error on FFI side
    toResult (Just 1) _ = Left FfiError
    -- error=2 corresponds to 'FfiError' on FFI side
    toResult (Just 2) _ = Left FfiError
    -- error=3 corresponds to 'ImplementationError' on FFI side
    toResult (Just 3) _ = Left FfiError
    toResult (Just 4) _ = Left DpopSyntaxError
    toResult (Just 5) _ = Left DpopTypError
    toResult (Just 6) _ = Left DpopUnsupportedAlgorithmError
    toResult (Just 7) _ = Left DpopInvalidSignatureError
    toResult (Just 8) _ = Left ClientIdMismatchError
    toResult (Just 9) _ = Left BackendNonceMismatchError
    toResult (Just 10) _ = Left HtuMismatchError
    toResult (Just 11) _ = Left HtmMismatchError
    toResult (Just 12) _ = Left MissingJtiError
    toResult (Just 13) _ = Left MissingChallengeError
    toResult (Just 14) _ = Left MissingIatError
    toResult (Just 15) _ = Left IatError
    toResult (Just 16) _ = Left MissingExpError
    toResult (Just 17) _ = Left ExpMismatchError
    toResult (Just 18) _ = Left ExpError
    -- this should also not happen, but apparently something went wrong
    toResult _ _ = Left FfiError

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
