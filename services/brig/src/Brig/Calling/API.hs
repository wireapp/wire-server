{-# LANGUAGE BlockArguments #-}

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

module Brig.Calling.API
  ( routesPublic,

    -- * Exposed for testing purposes
    newConfig,
    CallsConfigVersion (..),
    NoTurnServers,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.App
import Brig.Calling
import qualified Brig.Calling as Calling
import Brig.Calling.Internal
import Brig.Effects.SFT
import Brig.Options (ListAllSFTServers (..))
import qualified Brig.Options as Opt
import Control.Error (hush, throwE)
import Control.Lens
import Data.ByteString.Conversion
import Data.ByteString.Lens
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
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
import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified System.Logger.Class as Log
import qualified System.Random.MWC as MWC
import Wire.API.Call.Config (SFTServer)
import qualified Wire.API.Call.Config as Public
import Wire.Network.DNS.SRV (srvTarget)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

routesPublic :: Routes Doc.ApiBuilder (Handler r) ()
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

getCallsConfigV2H :: JSON ::: UserId ::: ConnId ::: Maybe (Range 1 10 Int) -> (Handler r) Response
getCallsConfigV2H (_ ::: uid ::: connid ::: limit) =
  json <$> getCallsConfigV2 uid connid limit

-- | ('UserId', 'ConnId' are required as args here to make sure this is an authenticated end-point.)
getCallsConfigV2 :: UserId -> ConnId -> Maybe (Range 1 10 Int) -> (Handler r) Public.RTCConfiguration
getCallsConfigV2 _ _ limit = do
  env <- view turnEnv
  staticUrl <- view $ settings . Opt.sftStaticUrl
  sftListAllServers <- fromMaybe Opt.HideAllSFTServers <$> view (settings . Opt.sftListAllServers)
  sftEnv' <- view sftEnv
  logger <- view applog
  manager <- view httpManager
  eitherConfig <-
    liftIO
      . runM @IO
      . loggerToTinyLog logger
      . interpretSFT manager
      . Polysemy.runError
      $ newConfig env turnServersV2 staticUrl sftEnv' limit sftListAllServers CallsConfigV2
  handleNoTurnServers eitherConfig

-- | Throws '500 Internal Server Error' when no turn servers are found. This is
-- done to keep backwards compatiblity, the previous code initialized an 'IORef'
-- with an 'error' so reading the 'IORef' threw a 500.
--
-- FUTUREWORK: Making this a '404 Not Found' would be more idiomatic, but this
-- should be done after consulting with client teams.
handleNoTurnServers :: Either NoTurnServers a -> (Handler r) a
handleNoTurnServers (Right x) = pure x
handleNoTurnServers (Left NoTurnServers) = do
  Log.err $ Log.msg (Log.val "Call config requested before TURN URIs could be discovered.")
  throwE $ StdError internalServerError

getCallsConfigH :: JSON ::: UserId ::: ConnId -> (Handler r) Response
getCallsConfigH (_ ::: uid ::: connid) =
  json <$> getCallsConfig uid connid

getCallsConfig :: UserId -> ConnId -> (Handler r) Public.RTCConfiguration
getCallsConfig _ _ = do
  env <- view turnEnv
  logger <- view applog
  manager <- view httpManager
  eitherConfig <-
    (dropTransport <$$>)
      . liftIO
      . runM @IO
      . loggerToTinyLog logger
      . interpretSFT manager
      . Polysemy.runError
      $ newConfig env turnServersV1 Nothing Nothing Nothing HideAllSFTServers CallsConfigDeprecated
  handleNoTurnServers eitherConfig
  where
    -- In order to avoid being backwards incompatible, remove the `transport` query param from the URIs
    dropTransport :: Public.RTCConfiguration -> Public.RTCConfiguration
    dropTransport =
      set
        (Public.rtcConfIceServers . traverse . Public.iceURLs . traverse . Public.turiTransport)
        Nothing

data CallsConfigVersion
  = CallsConfigDeprecated
  | CallsConfigV2

data NoTurnServers = NoTurnServers
  deriving (Show)

instance Exception NoTurnServers

-- | FUTUREWORK: It is not reflected in the function type the part of the
-- business logic that says that the SFT static URL parameter cannot be set at
-- the same time as the SFT environment parameter. See how to allow either none
-- to be set or only one of them (perhaps Data.These combined with error
-- handling).
newConfig ::
  Members [Embed IO, SFT, Polysemy.Error NoTurnServers] r =>
  Calling.TurnEnv ->
  Getter Calling.TurnEnv (IORef (Discovery (NonEmpty Public.TurnURI))) ->
  Maybe HttpsUrl ->
  Maybe SFTEnv ->
  Maybe (Range 1 10 Int) ->
  ListAllSFTServers ->
  CallsConfigVersion ->
  Sem r Public.RTCConfiguration
newConfig env discoveredServersL sftStaticUrl mSftEnv limit listAllServers version = do
  let (sha, secret, tTTL, cTTL, prng) = (env ^. turnSHA512, env ^. turnSecret, env ^. turnTokenTTL, env ^. turnConfigTTL, env ^. turnPrng)
  -- randomize list of servers (before limiting the list, to ensure not always the same servers are chosen if limit is set)
  randomizedUris <-
    liftIO
      . randomize
      =<< Polysemy.note NoTurnServers
        . discoveryToMaybe
      =<< readIORef (env ^. discoveredServersL)
  let limitedUris = case limit of
        Nothing -> randomizedUris
        Just lim -> limitedList randomizedUris lim
  -- randomize again (as limitedList partially re-orders uris)
  finalUris <- liftIO $ randomize limitedUris
  srvs <- for finalUris $ \uri -> do
    u <- liftIO $ genUsername tTTL prng
    pure $ Public.rtcIceServer (pure uri) u (computeCred sha secret u)

  let staticSft = pure . Public.sftServer <$> sftStaticUrl
  allSrvEntries <-
    fmap join $
      for mSftEnv $
        (unSFTServers <$$>) . fmap discoveryToMaybe . readIORef . sftServers
  srvEntries <- fmap join $
    for mSftEnv $ \actualSftEnv -> liftIO $ do
      let subsetLength = Calling.sftListLength actualSftEnv
      mapM (getRandomElements subsetLength) allSrvEntries

  mSftServersAll :: Maybe [SFTServer] <- case version of
    CallsConfigDeprecated -> pure Nothing
    CallsConfigV2 ->
      case (listAllServers, sftStaticUrl) of
        (HideAllSFTServers, _) -> pure Nothing
        (ListAllSFTServers, Nothing) -> pure . Just $ sftServerFromSrvTarget . srvTarget <$> maybe [] toList allSrvEntries
        (ListAllSFTServers, Just url) -> hush . unSFTGetResponse <$> sftGetAllServers url

  let mSftServers = staticSft <|> sftServerFromSrvTarget . srvTarget <$$> srvEntries
  pure $ Public.rtcConfiguration srvs mSftServers cTTL mSftServersAll
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
