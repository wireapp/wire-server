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
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lens
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List1 as List1
import Data.Misc (HttpsUrl)
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import Data.Text.Ascii (AsciiBase64, encodeBase64)
import Data.Text.Strict.Lens
import Data.Time.Clock.POSIX (getPOSIXTime)
import Imports hiding (head)
import Network.Wai (Response)
import Network.Wai.Predicate hiding (and, result, setStatus, (#))
import Network.Wai.Routing hiding (toList)
import Network.Wai.Utilities hiding (code, message)
import Network.Wai.Utilities.Swagger (document)
import OpenSSL.EVP.Digest (Digest, hmacBS)
import qualified System.Random.MWC as MWC
import qualified Wire.API.Call.Config as Public
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
  env <- liftIO =<< readIORef <$> view turnEnvV2
  staticUrl <- view $ settings . Opt.sftStaticUrl
  sftEnv' <- view sftEnv
  newConfig env staticUrl sftEnv' limit

getCallsConfigH :: JSON ::: UserId ::: ConnId -> Handler Response
getCallsConfigH (_ ::: uid ::: connid) =
  json <$> getCallsConfig uid connid

getCallsConfig :: UserId -> ConnId -> Handler Public.RTCConfiguration
getCallsConfig _ _ = do
  env <- liftIO =<< readIORef <$> view turnEnv
  dropTransport <$> newConfig env Nothing Nothing Nothing
  where
    -- In order to avoid being backwards incompatible, remove the `transport` query param from the URIs
    dropTransport :: Public.RTCConfiguration -> Public.RTCConfiguration
    dropTransport =
      set
        (Public.rtcConfIceServers . traverse . Public.iceURLs . traverse . Public.turiTransport)
        Nothing

newConfig :: MonadIO m => Calling.Env -> Maybe HttpsUrl -> Maybe SFTEnv -> Maybe (Range 1 10 Int) -> m Public.RTCConfiguration
newConfig env sftStaticUrl mSftEnv limit = do
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

  pure $ Public.rtcConfiguration srvs (staticSft <|> sftServerFromSrvTarget . srvTarget <$$> sftEntries) cTTL
  where
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
