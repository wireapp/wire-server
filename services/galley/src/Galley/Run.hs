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
    mkLogger,
    -- Exported for tests
    deleteFederationDomainRemote,
    deleteFederationDomainLocal,
    deleteFederationDomainOneOnOne,
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
import Data.Domain (Domain)
import Data.Id
import qualified Data.List.NonEmpty as N
import qualified Data.Map as Map
import Data.Metrics (Metrics)
import Data.Metrics.AWS (gaugeTokenRemaing)
import qualified Data.Metrics.Middleware as M
import Data.Metrics.Servant (servantPlusWAIPrometheusMiddleware)
import Data.Misc (portNumber)
import Data.Qualified
import qualified Data.Set as Set
import Data.Singletons
import Data.String.Conversions (cs)
import Data.Text (unpack)
import Data.Time (getCurrentTime)
import qualified Galley.API as API
import Galley.API.Action
import Galley.API.Error
import Galley.API.Federation
import Galley.API.Internal
import Galley.API.Util (getConversationWithError)
import Galley.App
import qualified Galley.App as App
import Galley.Aws (awsEnv)
import Galley.Cassandra
import Galley.Cassandra.Connection
import Galley.Data.Conversation.Types (convMetadata)
import qualified Galley.Effects.MemberStore as E
import Galley.Env
import Galley.Monad
import Galley.Options
import qualified Galley.Queue as Q
import Galley.Types.Conversations.Members
import Imports
import qualified Network.AMQP as AMQP
import Network.AMQP.Extended (RabbitMqHooks (RabbitMqHooks), openConnectionWithRetries)
import qualified Network.AMQP.Extended as AMQP
import qualified Network.AMQP.Types as AMQP
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.HTTP.Media.RenderHeader as HTTPMedia
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import qualified Network.Wai.Middleware.Gunzip as GZip
import qualified Network.Wai.Middleware.Gzip as GZip
import Network.Wai.Utilities.Server
import Polysemy.Error
import Servant hiding (route)
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), Scheme (Http))
import Servant.Client.Internal.HttpClient (defaultMakeClientRequest)
import qualified System.Logger as Log
import System.Logger.Extended (mkLogger)
import Util.Options
import Wire.API.Conversation (ConvType (ConnectConv, One2OneConv), cnvmType)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.FederationUpdate
import Wire.API.Routes.API
import Wire.API.Routes.FederationDomainConfig
import qualified Wire.API.Routes.Public.Galley as GalleyAPI
import Wire.API.Routes.Version.Wai

-- This type is used to tie the amqp sending and receiving message types together.
type MsgData = Domain

run :: Opts -> IO ()
run opts = lowerCodensity $ do
  l <- lift $ mkLogger (opts ^. optLogLevel) (opts ^. optLogNetStrings) (opts ^. optLogFormat)
  let Endpoint h p = opts ^. optBrig
  clientEnv <-
    liftIO $
      newManager defaultManagerSettings <&> \mgr ->
        ClientEnv mgr (BaseUrl Http (unpack h) (fromIntegral p) "") Nothing defaultMakeClientRequest
  ioref <- liftIO $ newIORef =<< getAllowedDomainsInitial l clientEnv
  (app, env) <- mkApp opts ioref l
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

  maybe
    -- Update federation domains without talking to rabbitmq
    -- This doesn't need a call back as there isn't anything
    -- we need to do beyond updating the IORef, which is done
    -- for us.
    (simpleFederationUpdate ioref clientEnv l)
    (complexFederationUpdate env clientEnv)
    $ opts ^. optRabbitMq

  void $ Codensity $ Async.withAsync $ runApp env deleteLoop
  void $ Codensity $ Async.withAsync $ runApp env refreshMetrics
  void $ Codensity $ Async.withAsync $ runApp env undefined
  lift $ finally (runSettingsWithShutdown settings app Nothing) (shutdown (env ^. cstate))

-- The simple update where rabbitmq isn't defined, so our processing is cut down a lot.
--
-- TODO: We still need to delete things, so we'll have to come up with some other processing
-- and non-volatile data storage to replace rabbit
simpleFederationUpdate :: IORef FederationDomainConfigs -> ClientEnv -> Log.Logger -> Codensity IO ()
simpleFederationUpdate ioref clientEnv l = void $
  Codensity $
    Async.withAsync $
      updateFedDomains' ioref clientEnv l $
        \_ _ -> pure ()

-- Complex handling. Most of the complexity comes from interweaving both rabbit queue handling
-- and the HTTP calls out to brig for the new lists of federation domains. Rabbit handles the
-- heavey lifting of ensuring single threaded processing of the domains to be deleted.
complexFederationUpdate ::
  Env ->
  ClientEnv ->
  RabbitMqOpts ->
  Codensity IO ()
complexFederationUpdate env clientEnv rmq = void $ Codensity $ Async.withAsync $ do
  -- This ioref is needed so that we can kill the async thread that
  -- is forked by updateFedDomains'
  threadRef <- newIORef Nothing
  let mqh = rmq ^. rmqHost
      mqp = rmq ^. rmqPort
      mqv = rmq ^. rmqVhost
      mqq = rmq ^. rmqQueue
  openConnectionWithRetries (env ^. applog) mqh mqp mqv $
    RabbitMqHooks
      { AMQP.onConnectionClose = do
          Log.info (env ^. applog) $ Log.msg @Text "AMQP connection closed"
          killForkedThread threadRef,
        AMQP.onChannelException = \e -> do
          Log.err (env ^. applog) $ Log.msg @Text "AMQP channel exception" . Log.field "exception" (show e)
          killForkedThread threadRef,
        AMQP.onNewChannel = \channel -> do
          -- NOTE: `amqp` uses ChanThreadKilledException to signal that this channel is closed
          -- This exception should _NOT_ be caught, or if it is it needs to be rethrown. This
          -- will kill the thread. This handler is then used to start a new one.
          ensureQueue channel mqq
          writeRabbitMq clientEnv env channel mqq threadRef
          readRabbitMq channel mqq env

          -- Keep this thread around until it is killed.
          forever $ threadDelay maxBound
      }
  where
    killForkedThread ref =
      readIORef ref
        >>= maybe
          (pure ())
          ( \t -> do
              Async.cancel t
              atomicWriteIORef ref Nothing
          )

-- Ensure that the queue exists and is single active consumer.
-- Queue declaration is idempotent
ensureQueue :: AMQP.Channel -> Text -> IO ()
ensureQueue channel mqq = do
  void $ AMQP.declareQueue channel $ AMQP.newQueue {AMQP.queueName = mqq, AMQP.queueHeaders = headers}
  where
    headers = AMQP.FieldTable $ Map.fromList [("x-single-active-consumer", AMQP.FVBool True)]

-- Update federation domains, write deleted domains to rabbitmq
-- Push this thread id somewhere so we can make sure it is killed with
-- this channel thread. We don't want to leak those resources.
writeRabbitMq ::
  ClientEnv ->
  Env ->
  AMQP.Channel ->
  Text ->
  IORef (Maybe (Async.Async ())) ->
  IO ()
writeRabbitMq clientEnv env channel mqq threadRef = do
  threadId <- updateFedDomains' ioref clientEnv (env ^. applog) $ \old new -> do
    let fromFedList = Set.fromList . remotes
        prevDoms = fromFedList old
        currDoms = fromFedList new
        deletedDomains = Set.difference prevDoms currDoms
    -- Write to the queue
    -- NOTE: This type must be compatible with what is being read from the queue.
    for_ deletedDomains $ \fedCfg -> do
      -- We're using the default exchange. This will deliver the
      -- message to the queue name used for the routing key
      void $
        AMQP.publishMsg channel "" mqq $
          AMQP.newMsg
            { AMQP.msgBody = Aeson.encode @MsgData $ domain fedCfg,
              AMQP.msgDeliveryMode = pure AMQP.Persistent
            }
  atomicWriteIORef threadRef $ pure threadId
  where
    ioref = env ^. fedDomains

-- Read messages from RabbitMQ, process the message, and ACK or NACK it as appropriate.
-- This is automatically killed by `amqp`, we don't need to handle it.
--
-- We can run this on every galley instance, and rabbitmq will handle the single
-- consumer constraint for us. This is done via the x-single-active-consumer header
-- that is set when the queue is created. When the active consumer disconnects for
-- whatever reason, rabbit will pick another of the subscribed clients to be the new
-- active consumer.
readRabbitMq :: AMQP.Channel -> Text -> Env -> IO ()
readRabbitMq channel mqq env = void $ AMQP.consumeMsgs channel mqq AMQP.Ack $ \(message, envelope) ->
  case Aeson.eitherDecode @MsgData (AMQP.msgBody message) of
    Left e -> do
      Log.err (env ^. applog) $ Log.msg @Text "Could not decode message from RabbitMQ" . Log.field "error" (show e)
      AMQP.nackEnv envelope
    Right dom -> do
      runApp env $ do
        deleteFederationDomainRemote dom
        deleteFederationDomainLocal dom
        deleteFederationDomainOneOnOne dom
      AMQP.ackEnv envelope

mkApp :: Opts -> IORef FederationDomainConfigs -> Log.Logger -> Codensity IO (Application, Env)
mkApp opts fedDoms logger =
  do
    metrics <- lift $ M.metrics
    env <- lift $ App.createEnv metrics opts logger fedDoms
    lift $ runClient (env ^. cstate) $ versionCheck schemaVersion
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

-- Remove remote members from local conversations
deleteFederationDomainRemote :: Domain -> App ()
deleteFederationDomainRemote dom = do
  env <- ask
  remoteUsers <- liftIO $ evalGalleyToIO env $ E.getRemoteMembersByDomain dom
  let lCnvMap = foldr insertIntoMap mempty remoteUsers
      localDomain = env ^. options . optSettings . setFederationDomain
  for_ (Map.toList lCnvMap) $ \(cnvId, rUsers) -> do
    let lCnvId = toLocalUnsafe localDomain cnvId
    -- This value contains an event that we might need to
    -- send out to all of the local clients that are a party
    -- to the conversation. However we also don't want to DOS
    -- clients. Maybe suppress and send out a bulk version?
    liftIO
      $ evalGalleyToIO env
      $ mapToRuntimeError @F.RemoveFromConversationError (InternalErrorWithDescription "Federation domain removal: Remove from conversation error")
        . mapToRuntimeError @'ConvNotFound (InternalErrorWithDescription "Federation domain removal: Conversation not found")
        . mapToRuntimeError @('ActionDenied 'RemoveConversationMember) (InternalErrorWithDescription "Federation domain removal: Action denied, remove conversation member")
        . mapToRuntimeError @'InvalidOperation (InternalErrorWithDescription "Federation domain removal: Invalid operation")
        . mapToRuntimeError @'NotATeamMember (InternalErrorWithDescription "Federation domain removal: Not a team member")
        . mapError @NoChanges (const (InternalErrorWithDescription "Federation domain removal: No changes"))
      -- This is allowed to send notifications to _local_ clients.
      -- But we are suppressing those events as we don't want to
      -- DOS our users if a large and deeply interconnected federation
      -- member is removed. Sending out hundreds or thousands of events
      -- to each client isn't something we want to be doing.
      $ do
        conv <- getConversationWithError lCnvId
        let lConv = toLocalUnsafe localDomain conv
        updateLocalConversationUserUnchecked
          @'ConversationRemoveMembersTag
          lConv
          undefined
          $ tUntagged . rmId <$> rUsers -- This field can be undefined as the path for ConversationRemoveMembersTag doens't use it

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

-- Remove local members from remote conversations
deleteFederationDomainLocal :: Domain -> App ()
deleteFederationDomainLocal dom = do
  env <- ask
  localUsers <- liftIO $ evalGalleyToIO env $ E.getLocalMembersByDomain dom
  -- As above, build the map so we can get all local users per conversation
  let rCnvMap = foldr insertIntoMap mempty localUsers
      localDomain = env ^. options . optSettings . setFederationDomain
  -- Process each user.
  for_ (Map.toList rCnvMap) $ \(cnv, lUsers) -> do
    liftIO $
      evalGalleyToIO env $
        mapError @NoChanges (const (InternalErrorWithDescription "No Changes: Could not remove a local member from a remote conversation.")) $
          do
            now <- liftIO $ getCurrentTime
            for_ lUsers $ \user -> do
              let lUser = toLocalUnsafe localDomain user
                  convUpdate =
                    F.ConversationUpdate
                      { cuTime = now,
                        cuOrigUserId = tUntagged lUser,
                        cuConvId = cnv,
                        cuAlreadyPresentUsers = mempty,
                        cuAction = SomeConversationAction (sing @'ConversationDeleteTag) ()
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

deleteFederationDomainOneOnOne :: Domain -> App ()
deleteFederationDomainOneOnOne dom = do
  env <- ask
  liftIO $ runClient (env ^. cstate) . deleteRemoteConnectionsByDomain $ dom