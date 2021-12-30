{-# LANGUAGE BlockArguments #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Calling.API
  ( routesPublic,
  )
where

import Brig.API.Handler
import Brig.App
import Brig.Calling
import qualified Brig.Calling as Calling
import Brig.Calling.Internal
import qualified Brig.Options as Opt
import Control.Lens
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Conversion
import Data.ByteString.Internal (ByteString (PS), w2c)
import Data.ByteString.Lens
import Data.ByteString.Unsafe (unsafeTake)
import Data.Either.Extra (eitherToMaybe)
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List1 as List1
import Data.Misc (HttpsUrl, Port (portNumber))
import Data.Range
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii (AsciiBase64, encodeBase64)
import Data.Text.Strict.Lens
import Data.Time.Clock.POSIX (getPOSIXTime)
import Imports hiding (head)
import Network.Connection
import Network.HTTP.Client hiding (Response)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.Wai (Response)
import Network.Wai.Predicate hiding (and, result, setStatus, (#))
import Network.Wai.Routing hiding (toList)
import Network.Wai.Utilities hiding (code, message)
import Network.Wai.Utilities.Swagger (document)
import OpenSSL.EVP.Digest (Digest, hmacBS)
import Polysemy (runM)
import Polysemy.TinyLog
import System.Logger (Logger)
import qualified System.Random.MWC as MWC
import Wire.API.Call.Config (SFTServer)
import qualified Wire.API.Call.Config as Public
import Wire.Network.DNS.Effect
import Wire.Network.DNS.SRV (srvTarget)

routesPublic :: Routes Doc.ApiBuilder Handler ()
routesPublic = do
  -- Deprecated endpoint, but still used by old clients.
  -- See https://github.com/zinfra/backend-issues/issues/1616 for context
  get "/calls/config" (continue getCallsConfigH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. header "Z-Connection"
  document "GET" "getCallsConfig" $ do
    Doc.deprecated
    Doc.summary
      "Retrieve TURN server addresses and credentials for \
      \ IP addresses, scheme `turn` and transport `udp` only "
    Doc.returns (Doc.ref Public.modelRtcConfiguration)
    Doc.response 200 "RTCConfiguration" Doc.end

  get "/calls/config/v2" (continue getCallsConfigV2H) $
    accept "application" "json"
      .&. header "Z-User"
      .&. header "Z-Connection"
      .&. opt (query "limit")
  document "GET" "getCallsConfigV2" $ do
    Doc.summary
      "Retrieve all TURN server addresses and credentials. \
      \Clients are expected to do a DNS lookup to resolve \
      \the IP addresses of the given hostnames "
    Doc.parameter Doc.Query "limit" Doc.int32' $ do
      Doc.description "Limit resulting list. Allowes values [1..10]"
      Doc.optional
    Doc.returns (Doc.ref Public.modelRtcConfiguration)
    Doc.response 200 "RTCConfiguration" Doc.end

getCallsConfigV2H :: JSON ::: UserId ::: ConnId ::: Maybe (Range 1 10 Int) -> Handler Response
getCallsConfigV2H (_ ::: uid ::: connid ::: limit) =
  json <$> getCallsConfigV2 uid connid limit

-- | ('UserId', 'ConnId' are required as args here to make sure this is an authenticated end-point.)
getCallsConfigV2 :: UserId -> ConnId -> Maybe (Range 1 10 Int) -> Handler Public.RTCConfiguration
getCallsConfigV2 _ _ limit = do
  env <- liftIO . readIORef =<< view turnEnvV2
  staticUrl <- view $ settings . Opt.sftStaticUrl
  sftEnv' <- view sftEnv
  logger <- view applog
  newConfig env staticUrl sftEnv' limit logger

getCallsConfigH :: JSON ::: UserId ::: ConnId -> Handler Response
getCallsConfigH (_ ::: uid ::: connid) =
  json <$> getCallsConfig uid connid

getCallsConfig :: UserId -> ConnId -> Handler Public.RTCConfiguration
getCallsConfig _ _ = do
  env <- liftIO . readIORef =<< view turnEnv
  logger <- view applog
  dropTransport <$> newConfig env Nothing Nothing Nothing logger
  where
    -- In order to avoid being backwards incompatible, remove the `transport` query param from the URIs
    dropTransport :: Public.RTCConfiguration -> Public.RTCConfiguration
    dropTransport =
      set
        (Public.rtcConfIceServers . traverse . Public.iceURLs . traverse . Public.turiTransport)
        Nothing

newConfig ::
  MonadIO m =>
  Calling.Env ->
  Maybe HttpsUrl ->
  Maybe SFTEnv ->
  Maybe (Range 1 10 Int) ->
  Logger ->
  m Public.RTCConfiguration
newConfig env sftStaticUrl mSftEnv limit logger = do
  let (sha, secret, tTTL, cTTL, prng) = (env ^. turnSHA512, env ^. turnSecret, env ^. turnTokenTTL, env ^. turnConfigTTL, env ^. turnPrng)
  -- randomize list of servers (before limiting the list, to ensure not always the same servers are chosen if limit is set)
  randomizedUris <- liftIO $ randomize (List1.toNonEmpty $ env ^. turnServers)
  let limitedUris = case limit of
        Nothing -> randomizedUris
        Just lim -> limitedList randomizedUris lim
  -- randomize again (as limitedList partially re-orders uris)
  finalUris <- liftIO $ randomize limitedUris
  srvs <- for finalUris $ \uri -> do
    u <- liftIO $ genUsername tTTL prng
    pure $ Public.rtcIceServer (uri :| []) u (computeCred sha secret u)

  let staticSft = (\url -> Public.sftServer url :| []) <$> sftStaticUrl
  sftEntries <- case mSftEnv of
    Nothing -> pure Nothing
    Just actualSftEnv -> do
      sftSrvEntries <- fmap discoveryToMaybe . readIORef . sftServers $ actualSftEnv
      let subsetLength = Calling.sftListLength actualSftEnv
      liftIO $ mapM (getRandomSFTServers subsetLength) sftSrvEntries

  let mSftServers = staticSft <|> sftServerFromSrvTarget . srvTarget <$$> sftEntries
  mSftServersAll :: Maybe (Maybe [SFTServer]) <- for mSftEnv $ \e -> liftIO $ do
    httpMan <-
      -- TODO: Put avoiding SSL checks behind a Brig configuration flag that is
      -- set only in testing and by default this flag is disabled
      let s = TLSSettingsSimple True False True
       in newTlsManagerWith $ mkManagerSettings s Nothing
    response <-
      runM
        . runTinyLog logger
        . runDNSLookupDefault
        . discoverSFTServersAll
        . Opt.unLookupDomain
        . sftLookupDomain
        $ e
    case response of
      Nothing -> pure $ Nothing @[SFTServer]
      Just ips -> fmap (eitherToMaybe @String @[SFTServer] . sequence) $
        for ips $ \ip -> do
          let req =
                parseRequest_ $
                  mconcat
                    [ "GET https://",
                      show ip,
                      ":",
                      show . portNumber . sftLookupPort $ e,
                      "/sft/url"
                    ]
          -- TODO: introduce an effect for talking to SFT. Perhaps this could be a
          -- part of an existing effect External.
          sftUrlResponse <- liftIO (responseBody <$> httpLbs req httpMan)
          pure @IO . fmap Public.sftServer . runParser' (parser @HttpsUrl) . cs . strip . cs $ sftUrlResponse

  pure $ Public.rtcConfiguration srvs mSftServers cTTL (join mSftServersAll)
  where
    -- FUTUREWORK: remove this adopted code once upgraded to bytestring >= 0.10.12.0
    strip :: BS8.ByteString -> BS8.ByteString
    strip = BS8.dropWhile isSpace . dropWhileEnd' isSpace
      where
        dropWhileEnd' :: (Char -> Bool) -> BS8.ByteString -> BS8.ByteString
        dropWhileEnd' f ps = unsafeTake (findFromEndUntil (not . f . w2c) ps) ps
        findFromEndUntil :: (Word8 -> Bool) -> BS8.ByteString -> Int
        findFromEndUntil f ps@(PS _ _ l) = case unsnoc ps of
          Nothing -> 0
          Just (b, c) ->
            if f c
              then l
              else findFromEndUntil f b

    limitedList :: NonEmpty Public.TurnURI -> Range 1 10 Int -> NonEmpty Public.TurnURI
    limitedList uris lim =
      -- assuming limitServers is safe with respect to the length of its return value
      -- (see property tests in brig-types)
      -- since the input is List1 and limit is in Range 1 10
      -- it should also be safe to assume the returning list has length >= 1
      NonEmpty.nonEmpty (Public.limitServers (NonEmpty.toList uris) (fromRange lim))
        & fromMaybe (error "newConfig:limitedList: empty list of servers")
    genUsername :: Word32 -> MWC.GenIO -> IO Public.TurnUsername
    genUsername ttl prng = do
      rnd <- view (packedBytes . utf8) <$> replicateM 16 (MWC.uniformR (97, 122) prng)
      t <- fromIntegral . (+ ttl) . round <$> getPOSIXTime
      pure $ Public.turnUsername t rnd
    computeCred :: Digest -> ByteString -> Public.TurnUsername -> AsciiBase64
    computeCred dig secret = encodeBase64 . hmacBS dig secret . toByteString'
