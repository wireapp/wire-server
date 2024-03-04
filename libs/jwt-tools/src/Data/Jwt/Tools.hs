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
    toResult,
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
    Handle (..),
    DisplayName (..),
    TeamId (..),
  )
where

import Control.Exception hiding (handle)
import Control.Monad.Trans.Except
import Data.ByteString.Conversion
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import Imports
import Network.HTTP.Types (StdMethod (..))

data HsResult

type ProofCStr = CString

type UserIdCStr = CString

type TeamIdCStr = CString

type HandleCStr = CString

type ClientIdWord64 = Word64

type DomainCStr = CString

type NonceCStr = CString

type UrlCStr = CString

type MethodCStr = CString

type MaxSkewSecsWord16 = Word16

type ExpiryEpochWord64 = Word64

type EpochWord64 = Word64

type BackendBundleCStr = CString

type DisplayNameCStr = CString

foreign import ccall unsafe "generate_dpop_access_token"
  generate_dpop_access_token ::
    ProofCStr ->
    UserIdCStr ->
    ClientIdWord64 ->
    HandleCStr ->
    DisplayNameCStr ->
    TeamIdCStr ->
    DomainCStr ->
    NonceCStr ->
    UrlCStr ->
    MethodCStr ->
    MaxSkewSecsWord16 ->
    ExpiryEpochWord64 ->
    EpochWord64 ->
    BackendBundleCStr ->
    IO (Ptr HsResult)

foreign import ccall unsafe "free_dpop_access_token" free_dpop_access_token :: Ptr HsResult -> IO ()

foreign import ccall unsafe "get_error" get_error :: Ptr HsResult -> Word8

foreign import ccall unsafe "get_token" get_token :: Ptr HsResult -> CString

generateDpopAccessTokenFfi ::
  ProofCStr ->
  UserIdCStr ->
  ClientIdWord64 ->
  HandleCStr ->
  DisplayNameCStr ->
  TeamIdCStr ->
  DomainCStr ->
  NonceCStr ->
  UrlCStr ->
  MethodCStr ->
  MaxSkewSecsWord16 ->
  ExpiryEpochWord64 ->
  EpochWord64 ->
  BackendBundleCStr ->
  IO (Maybe (Ptr HsResult))
generateDpopAccessTokenFfi dpopProof user client handle displayName tid domain nonce uri method maxSkewSecs expiration now backendKeys = do
  ptr <- generate_dpop_access_token dpopProof user client handle displayName tid domain nonce uri method maxSkewSecs expiration now backendKeys
  if ptr /= nullPtr
    then pure $ Just ptr
    else pure Nothing

getErrorFfi :: Ptr HsResult -> IO (Maybe Word8)
getErrorFfi ptr = do
  let err = get_error ptr
  if err /= 0
    then pure $ Just err
    else pure Nothing

getTokenFfi :: Ptr HsResult -> IO (Maybe String)
getTokenFfi ptr = do
  let tokenPtr = get_token ptr
  if tokenPtr /= nullPtr
    then Just <$> peekCString tokenPtr
    else pure Nothing

generateDpopToken ::
  (MonadIO m) =>
  Proof ->
  UserId ->
  ClientId ->
  Handle ->
  DisplayName ->
  TeamId ->
  Domain ->
  Nonce ->
  Uri ->
  StdMethod ->
  MaxSkewSecs ->
  ExpiryEpoch ->
  NowEpoch ->
  PemBundle ->
  ExceptT DPoPTokenGenerationError m ByteString
generateDpopToken dpopProof uid cid handle displayName tid domain nonce uri method maxSkewSecs maxExpiration now backendPubkeyBundle = do
  dpopProofCStr <- toCStr dpopProof
  uidCStr <- toCStr uid
  handleCStr <- toCStr handle
  displayNameCStr <- toCStr displayName
  tidCStr <- toCStr tid
  domainCStr <- toCStr domain
  nonceCStr <- toCStr nonce
  uriCStr <- toCStr uri
  methodCStr <- liftIO $ newCString $ cs $ methodToBS method
  backendPubkeyBundleCStr <- toCStr backendPubkeyBundle

  -- log all variable inputs (can comment in if need to generate new test data)
  -- traceM $ "proof = Proof " <> show (_unProof dpopProof)
  -- traceM $ "uid = UserId " <> show (_unUserId uid)
  -- traceM $ "nonce = Nonce " <> show (_unNonce nonce)
  -- traceM $ "expires = ExpiryEpoch " <> show (_unExpiryEpoch maxExpiration)
  -- traceM $ "handle = Handle " <> show (_unHandle handle)
  -- traceM $ "displayName = DisplayName " <> show (_unDisplayName displayName)
  -- traceM $ "tid = TeamId " <> show (_unTeamId tid)

  let before =
        generateDpopAccessTokenFfi
          dpopProofCStr
          uidCStr
          (_unClientId cid)
          handleCStr
          displayNameCStr
          tidCStr
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
          Just r -> toResult <$> getErrorFfi r <*> getTokenFfi r

  let free = maybe (pure ()) free_dpop_access_token

  ExceptT $ liftIO $ bracket before free mkAccessToken
  where
    toCStr :: forall a m. (ToByteString a, MonadIO m) => a -> m CString
    toCStr = liftIO . newCString . toStr
      where
        toStr :: a -> String
        toStr = cs . toByteString'

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

toResult :: Maybe Word8 -> Maybe String -> Either DPoPTokenGenerationError ByteString
-- the only valid cases are when the error=0 (meaning no error) or nothing and the token is not null
toResult (Just 0) (Just token) = Right $ cs token
toResult Nothing (Just token) = Right $ cs token
-- errors
toResult (Just errNo) _ = Left $ fromInt (fromIntegral errNo)
  where
    fromInt :: Int -> DPoPTokenGenerationError
    fromInt i =
      if i >= fromEnum @DPoPTokenGenerationError minBound && i <= fromEnum @DPoPTokenGenerationError maxBound
        then toEnum (fromIntegral i)
        else UnknownError
-- internal errors (unexpected)
toResult Nothing Nothing = Left UnknownError

newtype Proof = Proof {_unProof :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype UserId = UserId {_unUserId :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype ClientId = ClientId {_unClientId :: Word64}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype Handle = Handle {_unHandle :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

newtype TeamId = TeamId {_unTeamId :: ByteString}
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

newtype DisplayName = DisplayName {_unDisplayName :: ByteString}
  deriving (Eq, Show)
  deriving newtype (ToByteString)

data DPoPTokenGenerationError
  = NoError
  | -- | Unmapped error
    UnknownError
  | -- | Error at FFI boundary, probably related to raw pointer
    FfiError
  | -- | We messed up in rusty-jwt-tools
    ImplementationError
  | -- | DPoP token has an invalid syntax
    DpopSyntaxError
  | -- | DPoP header "typ" is not "dpop+jwt"
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
  | -- (exp) claim in DPoP token is sooner than now (with [max_skew_secs] leeway)
    Expired
  | -- userId supplied across the FFI is invalid
    InvalidUserId
  | -- Client DPoP token "nbf" claim is in the future
    NotYetValid
  | -- Bubbling up errors
    JwtSimpleError
  | -- Bubbling up errors
    RandError
  | -- Bubbling up errors
    Sec1Error
  | -- Bubbling up errors
    UrlParseError
  | -- Bubbling up errors
    UuidError
  | -- Bubbling up errors
    Utf8Error
  | -- Bubbling up errors
    Base64DecodeError
  | -- Bubbling up errors
    JsonError
  | -- Bubbling up errors
    InvalidJsonPath
  | -- Bubbling up errors
    JsonPathError
  | -- Bubbling up errors
    InvalidJwkThumbprint
  | -- Bubbling up errors
    MissingDpopHeader
  | -- Bubbling up errors
    MissingIssuer
  | -- Bubbling up errors
    DpopChallengeMismatch
  | -- Bubbling up errors
    DpopHtuMismatch
  | -- Bubbling up errors
    DpopHtmMismatch
  | -- Bubbling up errors
    InvalidBackendKeys
  | -- Bubbling up errors
    InvalidClientId
  | -- Bubbling up errors
    UnsupportedApiVersion
  | -- Bubbling up errors
    UnsupportedScope
  | -- Client handle does not match the supplied handle
    DpopHandleMismatch
  | -- Client team does not match the supplied team
    DpopTeamMismatch
  | --  Client display name does not match the supplied display name
    DpopDisplayNameMismatch
  deriving (Eq, Show, Generic, Bounded, Enum)
