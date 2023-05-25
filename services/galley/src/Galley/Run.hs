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
    updateFedDomainsCallback
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
import Galley.API.Internal
import Galley.App
import qualified Galley.App as App
import Galley.Aws (awsEnv)
import Galley.Cassandra
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
import qualified System.Logger as Log
import Util.Options
import Wire.API.Routes.API
import Wire.API.Routes.FederationDomainConfig
import qualified Wire.API.Routes.Public.Galley as GalleyAPI
import Wire.API.Routes.Version.Wai
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
import qualified Galley.Effects.MemberStore as E
import Data.Time (getCurrentTime)
import Wire.API.Conversation.Action
import Galley.API.Federation
import Data.Singletons
import Wire.API.Conversation (ConvType(One2OneConv), cnvmType, ConvType (ConnectConv, One2OneConv))
import Galley.Data.Conversation.Types (convMetadata)
import Galley.API.Util (getConversationWithError)
import Data.Domain (Domain)
import Galley.Cassandra.Connection

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
  -- void $ Codensity $ Async.withAsync $ runApp env updateFedDomains
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

-- Build the map, keyed by conversations to the list of members
insertIntoMap :: (ConvId, a) -> Map ConvId (N.NonEmpty a) -> Map ConvId (N.NonEmpty a)
insertIntoMap (cnvId, user) m = Map.alter (pure . maybe (pure user) (N.cons user)) cnvId m

deleteFederationDomainRemote :: Domain -> App ()
deleteFederationDomainRemote dom = do
  env <- ask
  remoteUsers <- liftIO $ evalGalleyToIO env $ E.getRemoteMembersByDomain dom
  let lCnvMap = foldr insertIntoMap mempty remoteUsers
  for_ (Map.toList lCnvMap) $ \(cnvId, rUsers) -> do
    let lCnvId = toLocalUnsafe dom cnvId
    -- This value contains an event that we might need to
    -- send out to all of the local clients that are a party
    -- to the conversation. However we also don't want to DOS
    -- clients. Maybe suppress and send out a bulk version?
    liftIO $ evalGalleyToIO env
      $ mapToRuntimeError @F.RemoveFromConversationError (InternalErrorWithDescription "Federation domain removal: Remove from conversation error")
      . mapToRuntimeError @'ConvNotFound (InternalErrorWithDescription "Federation domain removal: Conversation not found")
      . mapToRuntimeError @('ActionDenied 'RemoveConversationMember) (InternalErrorWithDescription  "Federation domain removal: Action denied, remove conversation member")
      . mapToRuntimeError @'InvalidOperation (InternalErrorWithDescription  "Federation domain removal: Invalid operation")
      . mapToRuntimeError @'NotATeamMember (InternalErrorWithDescription  "Federation domain removal: Not a team member")
      . mapError @NoChanges (const (InternalErrorWithDescription  "Federation domain removal: No changes"))
      -- This is allowed to send notifications to _local_ clients.
      -- But we are suppressing those events as we don't want to
      -- DOS our users if a large and deeply interconnected federation
      -- member is removed. Sending out hundreds or thousands of events
      -- to each client isn't something we want to be doing.
      $ do
        conv <- getConversationWithError lCnvId
        let lConv = toLocalUnsafe dom conv
        updateLocalConversationUserUnchecked
          @'ConversationRemoveMembersTag
          lConv
          undefined $ -- This field can be undefined as the path for ConversationRemoveMembersTag doens't use it
          tUntagged . rmId <$> rUsers
        -- Check if the conversation if type 2 or 3, one-on-one conversations.
        -- If it is, then we need to remove the entire conversation as users
        -- aren't able to delete those types of conversations themselves.
        -- Check that we are in a type 2 or a type 3 conversation
        when (cnvmType (convMetadata conv) `elem` [One2OneConv, ConnectConv]) $
          -- If we are, delete it.
          updateLocalConversationUserUnchecked
            @'ConversationDeleteTag
            lConv
            undefined
            ()

deleteFederationDomainLocal :: Domain -> App ()
deleteFederationDomainLocal dom = do
  env <- ask
  localUsers <- liftIO $ evalGalleyToIO env $ E.getLocalMembersByDomain dom
  -- As above, build the map so we can get all local users per conversation
  let rCnvMap = foldr insertIntoMap mempty localUsers
      localDomain = env ^. options . optSettings . setFederationDomain
  -- Process each user.
  for_ (Map.toList rCnvMap) $ \(cnv, lUsers) -> do
    liftIO $ evalGalleyToIO env
      $ mapError @NoChanges (const (InternalErrorWithDescription  "No Changes: Could not remove a local member from a remote conversation."))
      $ do
        now <- liftIO $ getCurrentTime
        for_ lUsers $ \user -> do
          let lUser = toLocalUnsafe localDomain user
              convUpdate = F.ConversationUpdate
                { cuTime = now
                , cuOrigUserId = tUntagged lUser
                , cuConvId = cnv
                , cuAlreadyPresentUsers = mempty
                , cuAction = SomeConversationAction (sing @'ConversationDeleteTag) ()
                }
          -- These functions are used directly rather than as part of a larger conversation
          -- delete function, as we don't have an originating user, and we can't send data
          -- to the remote backend.
          -- We don't need to check the conversation type here, as we can't tell the
          -- remote federation server to delete the conversation. They will have to do a
          -- similar processing run for removing the local domain from their federation list.
          onConversationUpdated dom convUpdate
          -- let rcnv = toRemoteUnsafe dom cnv
          -- notifyRemoteConversationAction lUser (qualifyAs rcnv convUpdate) Nothing
    

deleteFederationDomain :: Set FederationDomainConfig -> App ()
deleteFederationDomain deletedDomains = do
  for_ deletedDomains $ \fedDomCfg -> do
    -- https://wearezeta.atlassian.net/browse/FS-1179
    -- * Remove remote users for the given domain from all conversations owned by the current host
    -- * Remove all local users from remote conversations owned by the given domain.
    --   NOTE: This is NOT sent to other backends, as this information is not authoratative, but is
    --   good enough to tell local users about the federation connection being removed.
    -- * Delete all connections from local users to users for the remote domain
    -- Get all remote users for the given domain, along with conversation IDs that they are in
    let dom = domain fedDomCfg
    deleteFederationDomainRemote dom
    -- Get all local users for the given domain, along with remote conversation IDs that they are in
    deleteFederationDomainLocal dom
    -- Remove the remote one-on-one conversations between local members and remote members for the given domain.
    -- NOTE: We cannot tell the remote backend about these changes as we are no longer federated.
    env <- ask
    liftIO $ runClient (env ^. cstate) . deleteRemoteConnectionsByDomain $ dom
    
updateFedDomainsCallback :: FederationDomainConfigs -> FederationDomainConfigs -> App ()
updateFedDomainsCallback old new = do
  -- This code will only run when there is a change in the domain lists
  let fromFedList = Set.fromList . remotes
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
    deleteFederationDomain deletedDomains

