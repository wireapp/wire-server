{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.JwtTools where

import Brig.API.Types (CertEnrollmentError (..))
import Control.Monad.Trans.Except
import Data.ByteString.Conversion
import Data.Either.Extra
import Data.Handle (Handle, fromHandle)
import Data.Id
import Data.Jwt.Tools qualified as Jwt
import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Data.PEMKeys
import Imports
import Network.HTTP.Types (StdMethod (..))
import Network.HTTP.Types qualified as HTTP
import Polysemy
import Wire.API.MLS.Credential (ClientIdentity (..))
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken (DPoPAccessToken (..), Proof (..))
import Wire.API.User.Profile (Name (..))

data JwtTools m a where
  GenerateDPoPAccessToken ::
    -- | A DPoP proof in JWS Compact Serialization format
    -- Note that the proof consists of three runs of base64url characters
    -- (header, claims, signature) separated by period characters.
    Proof ->
    -- | The qualified client ID associated with the currently logged on user
    ClientIdentity ->
    -- | The user's handle
    Handle ->
    -- The user's display name
    Name ->
    -- | The user's team ID
    TeamId ->
    -- | The most recent DPoP nonce provided by the backend to the current client
    Nonce ->
    -- |  The HTTPS URI on the backend for the DPoP auth token endpoint
    HttpsUrl ->
    -- | The HTTPS method used on the backend for the DPoP auth token endpoint
    StdMethod ->
    -- | The maximum number of seconds of clock skew the implementation will allow
    Word16 ->
    -- | The expiration date and time, in seconds since "the epoch"
    Epoch ->
    -- | Current time in seconds since "the epoch"
    Epoch ->
    -- | PEM format concatenated private key and public key of the Wire backend
    PEMKeys ->
    JwtTools m (Either CertEnrollmentError DPoPAccessToken)

makeSem ''JwtTools

interpretJwtTools :: Member (Embed IO) r => Sem (JwtTools ': r) a -> Sem r a
interpretJwtTools = interpret $ \case
  GenerateDPoPAccessToken proof cid handle displayName tid nonce uri method skew ex now pem ->
    mapLeft RustError
      <$> runExceptT
        ( DPoPAccessToken
            <$> Jwt.generateDpopToken
              (Jwt.Proof (toByteString' proof))
              (Jwt.UserId (toByteString' (ciUser cid)))
              (Jwt.ClientId (clientToWord64 (ciClient cid)))
              (Jwt.Handle (toByteString' (urlEncode (fromHandle (handle)))))
              (Jwt.DisplayName (toByteString' (fromName displayName)))
              (Jwt.TeamId (toByteString' tid))
              (Jwt.Domain (toByteString' (ciDomain cid)))
              (Jwt.Nonce (toByteString' nonce))
              (Jwt.Uri (toByteString' uri))
              method
              (Jwt.MaxSkewSecs skew)
              (Jwt.ExpiryEpoch (epochNumber ex))
              (Jwt.NowEpoch (epochNumber now))
              (Jwt.PemBundle (toByteString' pem))
        )
  where
    urlEncode :: Text -> Text
    urlEncode = cs . HTTP.urlEncode False . cs
