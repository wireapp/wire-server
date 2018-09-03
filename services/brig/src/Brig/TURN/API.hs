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
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Random.Class
import Data.ByteString (ByteString)
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lens
import Data.Id
import Data.Range
import Data.IORef
import Data.List1 (List1)
import Data.Foldable (toList)
import Data.Text.Ascii (AsciiBase64, encodeBase64)
import Data.Text.Strict.Lens
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Traversable
import Data.Word
import OpenSSL.EVP.Digest (Digest, hmacBS)
import Network.Wai (Response)
import Network.Wai.Predicate hiding (setStatus, result, and, (#))
import Network.Wai.Routing hiding (toList)
import Network.Wai.Utilities hiding (message, code)
import Network.Wai.Utilities.Swagger (document)
import Prelude hiding (head)
import System.Random.Shuffle

import qualified Brig.Types.Swagger            as Doc
import qualified Brig.TURN                     as TURN
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
        Doc.summary "Retrieve TURN server addresses and credentials for \
                    \ IP addresses, scheme `turn` and transport `udp` only "
        Doc.returns (Doc.ref Doc.rtcConfiguration)
        Doc.response 200 "RTCConfiguration" Doc.end

    get "/calls/config/v2" (continue getCallsConfigV2) $
        accept "application" "json"
        .&. header "Z-User"
        .&. header "Z-Connection"
        .&. opt (query "limit")

    document "GET" "getCallsConfigV2" $ do
        Doc.summary "Retrieve all TURN server addresses and credentials. \
                    \Clients are expected to do a DNS lookup to resolve \
                    \the IP addresses of the given hostnames "
        Doc.parameter Doc.Query "limit" Doc.int32' $ do
            Doc.description "Limit resulting list. Allowes values [1..10]"
            Doc.optional
        Doc.returns (Doc.ref Doc.rtcConfiguration)
        Doc.response 200 "RTCConfiguration" Doc.end

getCallsConfigV2 :: JSON ::: UserId ::: ConnId ::: Maybe (Range 1 10 Int) -> Handler Response
getCallsConfigV2 (_ ::: _ ::: _ ::: limit ) = do
    env <- liftIO =<< readIORef <$> view turnEnvV2
    json <$> newConfig env limit

getCallsConfig :: JSON ::: UserId ::: ConnId -> Handler Response
getCallsConfig (_ ::: _ ::: _) = do
    env <- liftIO =<< readIORef <$> view turnEnv
    json . dropTransport <$> newConfig env Nothing
  where
    -- In order to avoid being backwards incompatible, remove the `transport` query param from the URIs
    dropTransport :: RTCConfiguration -> RTCConfiguration
    dropTransport = set (rtcConfIceServers . traverse . iceURLs . traverse . turiTransport) Nothing

newConfig :: MonadIO m => TURN.Env -> Maybe (Range 1 10 Int) -> m RTCConfiguration
newConfig env limit = do
    let (sha, secret, tTTL, cTTL, prng) = (env^.turnSHA512, env^.turnSecret, env^.turnTokenTTL, env^.turnConfigTTL, env^.turnPrng)
    -- randomize list of servers (before limiting the list, to ensure not always the same servers are chosen if limit is set)
    randomizedUris <- liftIO $ randomize (env^.turnServers)
    let limitedUris = case limit of
               Nothing  -> randomizedUris
               Just lim -> limitedList randomizedUris lim
    -- randomize again (as limitedList partially re-orders uris)
    finalUris <- liftIO $ randomize limitedUris
    srvs <- for finalUris $ \uri -> do
                u <- liftIO $ genUsername tTTL prng
                pure $ rtcIceServer (List1.singleton uri) u (computeCred sha secret u)
    pure $ rtcConfiguration srvs cTTL
  where
    -- NOTE: even though `shuffleM` works only for [a], input is List1 so it's
    --       safe to pattern match; ideally, we'd have `shuffleM` for `NonEmpty`
    randomize :: MonadRandom m => List1 TurnURI -> m (List1 TurnURI)
    randomize xs = do
        (f:fs) <- shuffleM (toList xs)
        return $ List1.list1 f fs

    limitedList :: List1 TurnURI -> Range 1 10 Int -> List1 TurnURI
    limitedList uris lim = do
        -- assuming limitServers is safe with respect to the length of its return value
        -- (see property tests in brig-types)
        -- since the input is List1 and limit is in Range 1 10
        -- it should also be safe to assume the returning list has length >= 1
        let (x:xs) = limitServers (toList uris) (fromRange lim)
        List1.list1 x xs

    genUsername :: Word32 -> MWC.GenIO -> IO TurnUsername
    genUsername ttl prng = do
        rnd <- view (packedBytes . utf8) <$> replicateM 16 (MWC.uniformR (97, 122) prng)
        t   <- fromIntegral . (+ ttl) . round <$> getPOSIXTime
        pure $ turnUsername t rnd

    computeCred :: Digest -> ByteString -> TurnUsername -> AsciiBase64
    computeCred dig secret = encodeBase64 . hmacBS dig secret . toByteString'
