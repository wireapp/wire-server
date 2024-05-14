{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Bots.Lib where

import qualified Bilge
import Bilge.Retry
import Bots.Types
import Cassandra as C
import Cassandra.Settings as C
import Control.Lens ((^.))
import Control.Lens.Combinators (view)
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion.To
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Data.Map as M
import Data.Misc (Fingerprint, HttpsUrl (..), Rsa)
import qualified Database.CQL.Protocol as CQL
import Galley.Cassandra.Instances ()
import Imports
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.OpenSSL as Ssl
import Network.HTTP.Types
import OpenSSL.EVP.Digest
import OpenSSL.Session as Ssl
import Options.Applicative
import Ssl.Util as Ssl
import qualified System.Logger as Log
import System.Logger.Message ((.=), (~~))
import URI.ByteString
import Wire.API.Routes.Internal.Galley.TeamsIntra (TeamStatus (..))

scanServiceTeam :: ClientState -> ConduitM () [ServiceTeamRow] IO ()
scanServiceTeam client =
  transPipe (runClient client) (paginateC selectAll (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    selectAll :: C.PrepQuery C.R () (CQL.TupleType ServiceTeamRow)
    selectAll =
      "SELECT provider, service, conv, team FROM service_user"

isTeamActive :: ClientState -> TeamId -> IO Bool
isTeamActive client tid = do
  mTeamTuple <- runClient client $ retry x1 (query1 lookupTeam (params One (Identity tid)))
  pure $ checkIfActive $ fmap CQL.asRecord mTeamTuple
  where
    lookupTeam :: PrepQuery R (Identity TeamId) (CQL.TupleType TeamRow)
    lookupTeam = "select team, deleted, status from team where team = ?"

    checkIfActive :: Maybe TeamRow -> Bool
    checkIfActive = all (\tr -> not tr.deleted && fromMaybe Active tr.status == Active)

lookupService :: ClientState -> ProviderId -> ServiceId -> IO (Maybe ServiceRow)
lookupService client pid sid = do
  mServiceTuple <- runClient client $ retry x1 (query1 q (params One (pid, sid)))
  pure $ fmap CQL.asRecord mServiceTuple
  where
    q :: PrepQuery R (ProviderId, ServiceId) (CQL.TupleType ServiceRow)
    q = "select base_url, auth_token, fingerprints, enabled from service where provider = ? AND id = ?"

type RequestCache = Map HttpsUrl Bool

reqToServiceOptionsReturns2xx :: Log.Logger -> ExternalServiceSettings -> ServiceRow -> IO Bool
reqToServiceOptionsReturns2xx logger extSrvSettings sr = do
  if not sr.enabled
    then pure False
    else do
      let authToken = toByteString' sr.authToken
      let httpsUrl = sr.baseUrl
      let HttpsUrl url = httpsUrl
      let CQL.Set fps = sr.fingerprints
      let runReq =
            recovering x3 httpHandlers $
              const $
                sendRequest fps $
                  Bilge.method OPTIONS
                    . maybe id Bilge.host (urlHost httpsUrl)
                    . maybe (Bilge.port 443) Bilge.port (urlPort httpsUrl)
                    . Bilge.paths [url ^. pathL, "bots"]
                    . Bilge.header "Authorization" ("Bearer " <> authToken)
                    . Bilge.timeout 5000
                    . Bilge.secure
      mStatusCode <-
        (Just . Bilge.statusCode <$> runReq) `catch` \(e :: SomeException) -> do
          Log.info logger $ "service" .= show (toByteString' sr.baseUrl) ~~ "error" .= show e
          pure $ Nothing
      case mStatusCode of
        Nothing -> do
          pure False
        Just sc ->
          if sc `div` 100 == 2
            then do
              Log.info logger $ "service" .= show (toByteString' sr.baseUrl) ~~ "status" .= sc ~~ Log.msg (Log.val "service returned 2xx")
              pure True
            else do
              Log.info logger $ "service" .= show (toByteString' sr.baseUrl) ~~ "status" .= maybe "n/a" show mStatusCode ~~ Log.msg (Log.val "service did not return 2xx")
              pure False
  where
    urlHost :: HttpsUrl -> Maybe ByteString
    urlHost (HttpsUrl u) = u ^. authorityL <&> view (authorityHostL . hostBSL)

    urlPort :: HttpsUrl -> Maybe Word16
    urlPort (HttpsUrl u) = do
      a <- u ^. authorityL
      p <- a ^. authorityPortL
      pure (fromIntegral (p ^. portNumberL))

    sendRequest :: [Fingerprint Rsa] -> (HTTP.Request -> HTTP.Request) -> IO (Bilge.Response ())
    sendRequest fprs reqBuilder = do
      withVerifiedSslConnection (extSrvSettings.verifyFingerprints fprs) extSrvSettings.manager reqBuilder $ \req -> do
        -- withVerifiedSslConnection (const $ pure ()) extSrvSettings.manager reqBuilder $ \req -> do
        HTTP.httpNoBody req extSrvSettings.manager

    x3 :: RetryPolicy
    x3 = limitRetries 3 <> constantDelay 1000000

lookupServiceName :: ClientState -> ProviderId -> ServiceId -> IO (Maybe Text)
lookupServiceName client pid sid =
  runClient client $ fmap runIdentity <$> retry x1 (query1 q (params One (pid, sid)))
  where
    q :: PrepQuery R (ProviderId, ServiceId) (Identity Text)
    q = "select name from service where provider = ? AND id = ?"

checkTeamAndServiceActive :: IORef RequestCache -> IORef (Map (ProviderId, ServiceId) (Maybe Text)) -> Log.Logger -> ExternalServiceSettings -> ClientState -> ClientState -> ServiceTeamRow -> IO TeamsWithService
checkTeamAndServiceActive reqCacheRef nameCacheRef logger extSrvSettings galleyClient brigClient sr = do
  active <- maybe (pure False) (isTeamActive galleyClient) sr.team
  if active
    then do
      mService <- lookupService galleyClient sr.provider sr.service
      case mService of
        Just svr -> do
          cache <- readIORef reqCacheRef
          isBackendActive <- case M.lookup svr.baseUrl cache of
            Nothing -> do
              b <- reqToServiceOptionsReturns2xx logger extSrvSettings svr
              writeIORef reqCacheRef (M.insert svr.baseUrl b cache)
              pure b
            Just b -> do
              Log.info logger $ Log.msg (Log.val "cache hit")
              pure b
          nameCache <- readIORef nameCacheRef
          mName <- case M.lookup (sr.provider, sr.service) nameCache of
            Nothing -> do
              mName <- lookupServiceName brigClient sr.provider sr.service
              writeIORef nameCacheRef (M.insert (sr.provider, sr.service) mName nameCache)
              pure mName
            Just mName -> pure mName
          pure $ toTeamsWithService (fromMaybe "not found" mName) svr.baseUrl isBackendActive sr
        Nothing ->
          pure $ TeamsWithService 1 mempty mempty mempty
    else do
      Log.debug logger $ "service_team" .= show sr ~~ Log.msg (Log.val "team is not active")
      pure $ TeamsWithService 1 mempty mempty mempty

process :: Log.Logger -> Maybe Int -> ExternalServiceSettings -> ClientState -> ClientState -> IO TeamsWithService
process logger limit extSrvSettings brigClient galleyClient = do
  reqCache :: IORef RequestCache <- newIORef M.empty
  nameCache :: IORef (Map (ProviderId, ServiceId) (Maybe Text)) <- newIORef M.empty
  runConduit $
    scanServiceTeam brigClient
      .| Conduit.concat
      .| (maybe (Conduit.filter (const True)) Conduit.take limit)
      .| Conduit.mapM (checkTeamAndServiceActive reqCache nameCache logger extSrvSettings galleyClient brigClient)
      .| forever (CL.isolate 1000 .| (Conduit.fold >>= yield))
      .| Conduit.takeWhile ((> 0) . entriesSearched)
      .| CL.scan (<>) mempty
        `fuseUpstream` Conduit.mapM_ (\r -> Log.info logger $ "entries_searched" .= show r.entriesSearched)

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  galleyClient <- initCas opts.galleyDb logger
  extSrvSettings <- initExtEnv logger
  res <- process logger opts.limit extSrvSettings brigClient galleyClient
  Log.info logger $ "services" .= servicesToCsv res.services
  Log.info logger $ "total_entries" .= show res.entriesSearched
  Log.info logger $ "number_of_teams_with_service" .= show (length res.teams)
  Log.info logger $ "number_of_conversations_with_service" .= show (length res.convs)
  where
    initLogger =
      Log.new
        . Log.setLogLevel Log.Info
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas settings l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts settings.host []
        . C.setPortNumber (fromIntegral settings.port)
        . C.setKeyspace settings.keyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "bots" <> progDesc "This program retrieves information about bots" <> fullDesc

initExtEnv :: Log.Logger -> IO ExternalServiceSettings
initExtEnv _logger = do
  ctx <- Ssl.context
  Ssl.contextSetVerificationMode ctx Ssl.VerifyNone
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextSetDefaultVerifyPaths ctx
  mgr <-
    HTTP.newManager
      (Ssl.opensslManagerSettings (pure ctx))
        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 10000000,
          HTTP.managerConnCount = 100
        }
  Just sha <- getDigestByName "SHA256"
  pure $ ExternalServiceSettings mgr (mkVerify sha)
  where
    mkVerify sha fprs =
      let pinset = map toByteString' fprs
       in verifyRsaFingerprint sha pinset
