{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.JwtTools where

import Control.Monad.Trans.Except
import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Data.PEMKeys
import Imports
import Jwt.Tools (generateDpopToken)
import Network.HTTP.Types (StdMethod (..))
import Polysemy
import Wire.API.MLS.Credential (ClientIdentity)
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken (DPoPAccessToken (..), DPoPTokenGenerationError, Proof (..))

data JwtTools m a where
  GenerateDPoPAccessToken ::
    -- | A DPoP proof in JWS Compact Serialization format
    -- Note that the proof consists of three runs of base64url characters
    -- (header, claims, signature) separated by period characters.
    Proof ->
    -- | The qualified client ID associated with the currently logged on user
    ClientIdentity ->
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
    JwtTools m (Either DPoPTokenGenerationError DPoPAccessToken)

makeSem ''JwtTools

interpretJwtToolsStub :: Members '[Embed IO] r => Sem (JwtTools ': r) a -> Sem r a
interpretJwtToolsStub = interpret $ \GenerateDPoPAccessToken {} -> do
  pure $ Right $ DPoPAccessToken "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

interpretJwtTools :: Members '[Embed IO] r => Sem (JwtTools ': r) a -> Sem r a
interpretJwtTools = interpret $ \(GenerateDPoPAccessToken proof ci n uri m skew ex now pem) -> do
  runExceptT $ generateDpopToken proof ci n uri m skew ex now pem
