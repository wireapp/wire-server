{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Data.Jwt.Tools
  ( generateDpopToken,
    DPoPTokenGenerationError (..),
    Proof (..),
    UserId (..),
    ClientId (..),
    Domain (..),
    Nonce (..),
    Uri (..),
    StdMethod (..),
    MaxSkewSecs (..),
    ExpiryEpoch (..),
    NowEpoch (..),
    PemBundle (..),
  )
where

import Control.Exception
import Control.Monad.Trans.Except
import Data.ByteString.Conversion
import Data.String.Conversions (cs)
import Foreign.C (CUChar (..))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Imports
import Network.HTTP.Types (StdMethod (..))

data JwtResponse

type ProofCStr = CString

type UserIdCStr = CString

type ClientIdWord16 = Word16

type DomainCStr = CString

type NonceCStr = CString

type UrlCStr = CString

type MethodCStr = CString

type MaxSkewSecsWord16 = Word16

type ExpiryEpochWord64 = Word64

type EpochWord64 = Word64

type BackendBundleCStr = CString

foreign import ccall unsafe "generate_dpop_access_token"
  generate_dpop_access_token ::
    ProofCStr ->
    UserIdCStr ->
    ClientIdWord16 ->
    DomainCStr ->
    NonceCStr ->
    UrlCStr ->
    MethodCStr ->
    MaxSkewSecsWord16 ->
    ExpiryEpochWord64 ->
    EpochWord64 ->
    BackendBundleCStr ->
    IO (Ptr JwtResponse)

foreign import ccall unsafe "free_dpop_access_token" free_dpop_access_token :: Ptr JwtResponse -> IO ()

foreign import ccall unsafe "get_error" get_error :: Ptr JwtResponse -> Ptr CUChar

foreign import ccall unsafe "get_token" get_token :: Ptr JwtResponse -> CString

createToken ::
  ProofCStr ->
  UserIdCStr ->
  ClientIdWord16 ->
  DomainCStr ->
  NonceCStr ->
  UrlCStr ->
  MethodCStr ->
  MaxSkewSecsWord16 ->
  ExpiryEpochWord64 ->
  EpochWord64 ->
  BackendBundleCStr ->
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
  UserId ->
  ClientId ->
  Domain ->
  Nonce ->
  Uri ->
  StdMethod ->
  MaxSkewSecs ->
  ExpiryEpoch ->
  NowEpoch ->
  PemBundle ->
  ExceptT DPoPTokenGenerationError m ByteString
generateDpopToken dpopProof uid cid domain nonce uri method maxSkewSecs maxExpiration now backendPubkeyBundle = do
  dpopProofCStr <- toCStr dpopProof
  uidCStr <- toCStr uid
  domainCStr <- toCStr domain
  nonceCStr <- toCStr nonce
  uriCStr <- toCStr uri
  methodCStr <- liftIO $ newCString $ cs $ methodToBS method
  backendPubkeyBundleCStr <- toCStr backendPubkeyBundle

  let before =
        createToken
          dpopProofCStr
          uidCStr
          (_unClientId cid)
          domainCStr
          nonceCStr
          uriCStr
          methodCStr
          (_unMaxSkewSecs maxSkewSecs)
          (_unExpiryEpoch maxExpiration)
          (_unNowEpoch now)
          backendPubkeyBundleCStr

  let mkAccessToken response = do
        case response of
          Nothing -> pure $ Left FfiError
          Just r -> toResult <$> getError r <*> getToken r

  let free = maybe (pure ()) free_dpop_access_token

  ExceptT $ liftIO $ bracket before free mkAccessToken
  where
    toResult :: Maybe Word8 -> Maybe String -> Either DPoPTokenGenerationError ByteString
    -- the only valid case is when the error=0 (meaning no error) and the token is not null
    toResult Nothing (Just token) = Right $ cs token
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

newtype Proof = Proof {_unProof :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype UserId = UserId {_unUserId :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype ClientId = ClientId {_unClientId :: Word16}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype Domain = Domain {_unDomain :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype Nonce = Nonce {_unNonce :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype Uri = Uri {_unUri :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype MaxSkewSecs = MaxSkewSecs {_unMaxSkewSecs :: Word16}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype ExpiryEpoch = ExpiryEpoch {_unExpiryEpoch :: Word64}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype NowEpoch = NowEpoch {_unNowEpoch :: Word64}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype PemBundle = PemBundle {_unPemBundle :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

data DPoPTokenGenerationError
  = -- | DPoP token has an invalid syntax
    DpopSyntaxError
  | -- | DPoP header 'typ' is not 'dpop+jwt'
    DpopTypError
  | -- | DPoP signature algorithm (alg) in JWT header is not a supported algorithm (ES256, ES384, Ed25519)
    DpopUnsupportedAlgorithmError
  | -- | DPoP signature does not correspond to the public key (jwk) in the JWT header
    DpopInvalidSignatureError
  | -- | [client_id] does not correspond to the (sub) claim expressed as URI
    ClientIdMismatchError
  | -- | [backend_nonce] does not correspond to the (nonce) claim in DPoP token (base64url encoded)
    BackendNonceMismatchError
  | -- | [uri] does not correspond to the (htu) claim in DPoP token
    HtuMismatchError
  | -- | method does not correspond to the (htm) claim in DPoP token
    HtmMismatchError
  | -- | (jti) claim is absent in DPoP token
    MissingJtiError
  | -- | (chal) claim is absent in DPoP token
    MissingChallengeError
  | -- | (iat) claim is absent in DPoP token
    MissingIatError
  | -- | (iat) claim in DPoP token is not earlier of now (with [max_skew_secs] leeway)
    IatError
  | -- | (exp) claim is absent in DPoP token
    MissingExpError
  | -- | (exp) claim in DPoP token is larger than supplied [max_expiration]
    ExpMismatchError
  | -- | (exp) claim in DPoP token is sooner than now (with [max_skew_secs] leeway)
    ExpError
  | -- | the client id has an invalid syntax
    ClientIdSyntaxError
  | -- | Error at FFI boundary, probably related to raw pointer
    FfiError
  deriving (Eq, Show, Generic)
