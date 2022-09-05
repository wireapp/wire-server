{-# LANGUAGE TemplateHaskell #-}

module Brig.Sem.JwtTools where

import Data.Misc (HttpsUrl)
import Data.Nonce (Nonce)
import Imports
import Network.HTTP.Types (StdMethod (..))
import Polysemy
import Wire.API.MLS.Credential (ClientIdentity)
import Wire.API.MLS.Epoch (Epoch (..))
import Wire.API.User.Client.DPoPAccessToken (DPoPAccessToken (..), DPoPTokenGenerationError, Proof (..))

data JwtTools m a where
  GenerateDPoPAccessToken ::
    Proof ->
    ClientIdentity ->
    Nonce ->
    HttpsUrl ->
    StdMethod ->
    -- | The maximum number of seconds of clock skew the implementation will allow
    Word16 ->
    -- | The expiration date and time, in seconds since "the epoch"
    Word64 ->
    -- | Current time in seconds since "the epoch"
    Epoch ->
    -- | PEM format concatenated private key and public key of the Wire backend
    ByteString ->
    JwtTools m (Either DPoPTokenGenerationError DPoPAccessToken)

makeSem ''JwtTools

interpretJwtToolsStub :: Members '[Embed IO] r => Sem (JwtTools ': r) a -> Sem r a
interpretJwtToolsStub = interpret $ \(GenerateDPoPAccessToken _ _ _ _ _ _ _ _ _) -> do
  pure $ Right $ DPoPAccessToken "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
