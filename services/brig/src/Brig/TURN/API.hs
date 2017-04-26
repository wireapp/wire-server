{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Brig.TURN.API (routes) where

import Brig.App
import Brig.TURN hiding (Env)
import Brig.Types.TURN 
import Brig.API.Handler
import Control.Lens (view, (^.), (#))
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lens
import Data.Id
import Data.IORef
import Data.Text.Ascii (AsciiBase64, encodeBase64)
import Data.Text.Strict.Lens
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Traversable
import Data.Word
import OpenSSL.EVP.Digest (Digest, hmacBS)
import Network.Wai (Response)
import Network.Wai.Predicate hiding (setStatus, result, and, (#))
import Network.Wai.Routing
import Network.Wai.Utilities hiding (message, code)
import Network.Wai.Utilities.Swagger (document)
import Prelude hiding (head)

import qualified Brig.Types.Swagger            as Doc
import qualified Data.List1                    as List1
import qualified Data.Swagger.Build.Api        as Doc
import qualified System.Random.MWC             as MWC

routes :: Routes Doc.ApiBuilder Handler ()
routes = do

    get "/calls/config" (continue getCallsConfig) $
        accept "application" "json"
        .&. header "Z-User"
        .&. header "Z-Connection"

    document "GET" "getCallsConfig" $ do
        Doc.summary "Retrieve TURN server addresses and credentials."
        Doc.returns (Doc.ref Doc.rtcConfiguration)
        Doc.response 200 "RTCConfiguration" Doc.end

getCallsConfig :: JSON ::: UserId ::: ConnId -> Handler Response
getCallsConfig (_ ::: _ ::: _) = json <$> lift newConfig
  where
    newConfig :: (MonadIO m, MonadReader Env m) => m RTCConfiguration
    newConfig = do
        env  <- liftIO =<< readIORef <$> view turnEnv
        srvs <- for (env^.turnServers) $ \srv -> do
                    u <- liftIO $ genUsername (env^.turnTTL) (env^.turnPrng)
                    pure $
                        rtcIceServer (List1.singleton $ turnURI (_TurnHost # srv) 3478)
                                     u
                                     (hmac (view turnSHA512 env) (view turnSecret env) u)
        pure $ rtcConfiguration srvs (Just (view turnTTL env))
      where
        genUsername :: Word32 -> MWC.GenIO -> IO TurnUsername
        genUsername ttl prng = do
            rnd <- view (packedBytes . utf8) <$> replicateM 16 (MWC.uniformR (97, 122) prng)
            t   <- fromIntegral . (+ ttl) . round <$> getPOSIXTime
            pure $ turnUsername t rnd

        hmac :: Digest -> ByteString -> TurnUsername -> AsciiBase64
        hmac dig secret = encodeBase64 . hmacBS dig secret . toByteString'
