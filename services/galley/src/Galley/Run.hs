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
{-# LANGUAGE NumericUnderscores #-}

module Galley.Run
  ( run,
    mkApp,
  )
where

import AWS.Util (readAuthExpiration)
import qualified Amazonka as AWS
import Bilge.Request (requestIdName)
import Cassandra (runClient, shutdown)
import Cassandra.Schema (versionCheck)
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Codensity
import qualified Data.Aeson as Aeson
import Data.Default
import Data.Id
import qualified Data.Map as Map
import Data.Metrics (Metrics)
import Data.Metrics.AWS (gaugeTokenRemaing)
import qualified Data.Metrics.Middleware as M
import Data.Metrics.Servant (servantPlusWAIPrometheusMiddleware)
import Data.Misc (portNumber)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (unpack)
import qualified Galley.API as API
import Galley.API.Federation (FederationAPI, federationSitemap)
import Galley.API.Internal
import Galley.App
import qualified Galley.App as App
import Galley.Aws (awsEnv)
import Galley.Cassandra
import Galley.Env (fedDomains)
import Galley.Monad
import Galley.Options
import qualified Galley.Queue as Q
import Imports
import qualified Network.HTTP.Media.RenderHeader as HTTPMedia
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import Servant hiding (route)
import Servant.Client
  ( BaseUrl (BaseUrl),
    ClientEnv (ClientEnv),
    Scheme (Http),
    defaultMakeClientRequest,
  )
import qualified System.Logger as Log
import Util.Options
import Wire.API.FederationUpdate
import Wire.API.Routes.API
import Wire.API.Routes.FederationDomainConfig
import qualified Wire.API.Routes.Public.Galley as GalleyAPI
import Wire.API.Routes.Version.Wai
import qualified Galley.Effects.MemberStore as E
import qualified Data.List.NonEmpty as N
import Galley.API.Action
import Galley.Types.Conversations.Members
import Data.Qualified
import Wire.API.Error.Galley
import Wire.API.Conversation.Role
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Error
import Polysemy.Error
import Galley.API.Error

run :: Opts -> IO ()
run opts = lowerCodensity $ do
  (app, env) <- mkApp opts
  settings <-
    lift $
      newSettings $
        defaultServer
          (unpack $ opts ^. optGalley . epHost)
          (portNumber $ fromIntegral $ opts ^. optGalley . epPort)
          (env ^. App.applog)
          (env ^. monitor)

  forM_ (env ^. aEnv) $ \aws ->
    void $ Codensity $ Async.withAsync $ collectAuthMetrics (env ^. monitor) (aws ^. awsEnv)
  void $ Codensity $ Async.withAsync $ runApp env updateFedDomains
  void $ Codensity $ Async.withAsync $ runApp env deleteLoop
  void $ Codensity $ Async.withAsync $ runApp env refreshMetrics
  void $ Codensity $ Async.withAsync $ runApp env undefined
  lift $ finally (runSettingsWithShutdown settings app Nothing) (shutdown (env ^. cstate))

mkApp :: Opts -> Codensity IO (Application, Env)
mkApp opts =
  do
    metrics <- lift $ M.metrics
    env <- lift $ App.createEnv metrics opts
    lift $ runClient (env ^. cstate) $ versionCheck schemaVersion

    let logger = env ^. App.applog

    let middlewares =
          versionMiddleware (opts ^. optSettings . setDisabledAPIVersions . traverse)
            . servantPlusWAIPrometheusMiddleware API.sitemap (Proxy @CombinedAPI)
            . GZip.gunzip
            . GZip.gzip GZip.def
            . catchErrors logger [Right metrics]
    Codensity $ \k -> finally (k ()) $ do
      Log.info logger $ Log.msg @Text "Galley application finished."
      Log.flush logger
      Log.close logger
    pure (middlewares $ servantApp env, env)
  where
    rtree = compile API.sitemap
    runGalley e r k = evalGalleyToIO e (route rtree r k)
    -- the servant API wraps the one defined using wai-routing
    servantApp e0 r =
      let e = reqId .~ lookupReqId r $ e0
       in Servant.serveWithContext
            (Proxy @CombinedAPI)
            ( view (options . optSettings . setFederationDomain) e
                :. customFormatters
                :. Servant.EmptyContext
            )
            ( hoistAPIHandler (toServantHandler e) API.servantSitemap
                :<|> hoistAPIHandler (toServantHandler e) internalAPI
                :<|> hoistServerWithDomain @FederationAPI (toServantHandler e) federationSitemap
                :<|> Servant.Tagged (runGalley e)
            )
            r

    lookupReqId :: Request -> RequestId
    lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders

customFormatters :: Servant.ErrorFormatters
customFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = bodyParserErrorFormatter'
    }

bodyParserErrorFormatter' :: Servant.ErrorFormatter
bodyParserErrorFormatter' _ _ errMsg =
  Servant.ServerError
    { Servant.errHTTPCode = HTTP.statusCode HTTP.status400,
      Servant.errReasonPhrase = cs $ HTTP.statusMessage HTTP.status400,
      Servant.errBody =
        Aeson.encode $
          Aeson.object
            [ "code" Aeson..= Aeson.Number 400,
              "message" Aeson..= errMsg,
              "label" Aeson..= ("bad-request" :: Text)
            ],
      Servant.errHeaders = [(HTTP.hContentType, HTTPMedia.renderHeader (Servant.contentType (Proxy @Servant.JSON)))]
    }

type CombinedAPI =
  GalleyAPI.ServantAPI
    :<|> InternalAPI
    :<|> FederationAPI
    :<|> Servant.Raw

refreshMetrics :: App ()
refreshMetrics = do
  m <- view monitor
  q <- view deleteQueue
  safeForever "refreshMetrics" $ do
    n <- Q.len q
    M.gaugeSet (fromIntegral n) (M.path "galley.deletequeue.len") m
    threadDelay 1000000

collectAuthMetrics :: MonadIO m => Metrics -> AWS.Env -> m ()
collectAuthMetrics m env = do
  liftIO $
    forever $ do
      mbRemaining <- readAuthExpiration env
      gaugeTokenRemaing m mbRemaining
      threadDelay 1_000_000

updateFedDomains :: App ()
updateFedDomains = do
  env <- ask
  let ioref              = env ^. fedDomains
      logger             = env ^. applog
      manager'           = env ^. manager
      Endpoint host port = env ^. brig
      baseUrl   = BaseUrl Http (unpack host) (fromIntegral port) ""
      clientEnv = ClientEnv manager' baseUrl Nothing defaultMakeClientRequest
  liftIO $ getAllowedDomainsLoop logger clientEnv ioref $ callback env
  where
    callback env old new = do
      -- TODO: perform the database updates here
      -- This code will only run when there is a change in the domain lists
      let fromFedList = Set.fromList . fromFederationDomainConfigs
          prevDoms = fromFedList old
          currDoms = fromFedList new
      unless (prevDoms == currDoms) $ do
        -- Perform updates before rewriting the tvar
        -- This means that if the update fails on a
        -- particular invocation, it can be run again
        -- on the next firing as it isn't likely that
        -- the domain list is changing frequently.
        -- FS-1179 is handling this part.
        let deletedDomains = Set.difference prevDoms currDoms
            addedDomains = Set.difference currDoms prevDoms
        for_ deletedDomains $ \fedDomCfg -> do
          -- https://wearezeta.atlassian.net/browse/FS-1179
          -- TODO
          -- * Remove remote users for the given domain from all conversations owned by the current host
          -- * Remove all local users from remote conversations owned by the given domain.
          --   NOTE: This is NOT sent to other backends, as this information is not authoratative, but is
          --   good enough to tell local users about the federation connection being removed.
          -- * Delete all connections from local users to users for the remote domain.
          -- Get all remote users for the given domain, along with conversation IDs that they are in
          remoteUsers <- liftIO $ evalGalleyToIO env $ E.getRemoteMembersByDomain $ domain fedDomCfg
          let cnvMap :: Map ConvId (N.NonEmpty RemoteMember)
              cnvMap = foldr insertIntoMap mempty remoteUsers
              -- Build the map, keyed by conversations to the list of remote members
              insertIntoMap (cnvId, user) m = Map.alter (pure . maybe (pure user) (N.cons user)) cnvId m
          for_ (Map.toList cnvMap) $ \(cnv, rUsers) -> do
            -- This value contains an event that we might need to
            -- send out to all of the local clients that are a party
            -- to the conversation. However we also don't want to DOS
            -- clients. Maybe suppress and send out a bulk version?
            _res <- liftIO $ evalGalleyToIO env
              -- TODO: Are these the right error types we should be using?
              -- TODO: We are restricted to the errors listed in GalleyEffects,
              -- TODO: and none of those seem like a great fit.
              $ mapToRuntimeError @F.RemoveFromConversationError (InternalErrorWithDescription "Foo")
              . mapToRuntimeError @'ConvNotFound (InternalErrorWithDescription "Bar")
              . mapToRuntimeError @('ActionDenied 'RemoveConversationMember) (InternalErrorWithDescription  "Baz")
              . mapToRuntimeError @'InvalidOperation (InternalErrorWithDescription  "Qux")
              . mapError @NoChanges (const (InternalErrorWithDescription  "Qwe"))
              $ updateLocalConversation
                @'ConversationRemoveMembersTag
                (toLocalUnsafe (domain fedDomCfg) cnv)
                undefined
                Nothing $
                tUntagged . rmId <$> rUsers
            pure ()
        for_ addedDomains $ \_domain -> do
          pure ()
