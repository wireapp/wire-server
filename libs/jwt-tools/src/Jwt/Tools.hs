{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

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

data JwtResponse = JwtResponse

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
    _ -> throwE InvalidClientId
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
          Nothing -> pure $ Left UnknownError
          Just r -> do
            mErr <- getError r
            print mErr
            mToken <- getToken r
            pure $ toResult mErr mToken

  let free = maybe (pure ()) free_dpop_access_token

  ExceptT $ liftIO $ bracket before free mkAccessToken
  where
    toResult :: Maybe Word8 -> Maybe String -> Either DPoPTokenGenerationError DPoPAccessToken
    toResult (Just err) _ = maybe (Left UnknownError) Left (mapError err)
    toResult _ (Just token) = Right $ DPoPAccessToken $ cs token
    toResult _ _ = Left UnknownError

    mapError :: Word8 -> Maybe DPoPTokenGenerationError
    mapError 0 = Nothing
    mapError 1 = Just BadProof
    mapError 2 = Just BadDPoPHeader
    mapError 3 = Just AlgNotSupported
    mapError 4 = Just BadSignature
    mapError 5 = Just BadQualifiedClientId
    mapError 6 = Just BadNonce
    mapError 7 = Just BadUri
    mapError 8 = Just BadMethod
    mapError 9 = Just JtiClaimMissing
    mapError 10 = Just ChalClaimMissing
    mapError 11 = Just BadIatClaim
    mapError 12 = Just BadExpClaim
    mapError 13 = Just BadExpClaim
    mapError _ = Just UnknownError

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
