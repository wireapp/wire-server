{-# LANGUAGE StrictData #-}

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

module Galley.App
  ( -- * Environment
    Env,
    reqId,
    options,
    applog,
    manager,
    federator,
    brig,
    cstate,
    deleteQueue,
    createEnv,
    extEnv,
    aEnv,
    ExtEnv (..),
    extGetManager,

    -- * Running Galley effects
    GalleyEffects,
    evalGalleyToIO,
    ask,
    DeleteItem (..),
    toServantHandler,
  )
where

import Bilge hiding (Request, header, host, options, port, statusCode, statusMessage)
import Cassandra hiding (Set)
import Cassandra.Util (initCassandraForService)
import Control.Error hiding (err)
import Control.Lens hiding ((.=))
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Galley.API.Error
import Galley.Aws qualified as Aws
import Galley.Cassandra.Client
import Galley.Cassandra.Code
import Galley.Cassandra.CustomBackend
import Galley.Cassandra.LegalHold
import Galley.Cassandra.Proposal
import Galley.Cassandra.SearchVisibility
import Galley.Cassandra.Services
import Galley.Cassandra.Team
import Galley.Cassandra.TeamFeatures
import Galley.Cassandra.TeamNotifications
import Galley.Effects
import Galley.Effects.FireAndForget
import Galley.Env
import Galley.External
import Galley.Intra.BackendNotificationQueue
import Galley.Intra.Effects
import Galley.Intra.Federator
import Galley.Keys
import Galley.Options hiding (brig, endpoint, federator)
import Galley.Options qualified as O
import Galley.Queue
import Galley.Queue qualified as Q
import Galley.TeamSubsystem (interpretTeamSubsystem)
import Galley.Types.Teams
import HTTP2.Client.Manager (Http2Manager, http2ManagerWithSSLCtx)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended (initPostgresPool)
import Imports hiding (forkIO)
import Network.AMQP.Extended (mkRabbitMqChannelMVar)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai.Utilities.JSONResponse
import OpenSSL.Session as Ssl
import Polysemy
import Polysemy.Async
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Internal (Append)
import Polysemy.Resource
import Polysemy.TinyLog qualified as P
import Servant qualified
import Ssl.Util
import System.Logger qualified as Log
import System.Logger.Class (Logger)
import System.Logger.Extended qualified as Logger
import UnliftIO.Exception qualified as UnliftIO
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Federation.Error
import Wire.API.Team.Collaborator
import Wire.API.Team.Feature
import Wire.BrigAPIAccess.Rpc
import Wire.ConversationStore.Cassandra (interpretConversationStoreToCassandra)
import Wire.Error
import Wire.GundeckAPIAccess (runGundeckAPIAccess)
import Wire.HashPassword.Interpreter
import Wire.ListItems.ConversationIds.Cassandra
import Wire.NotificationSubsystem.Interpreter (runNotificationSubsystemGundeck)
import Wire.ParseException
import Wire.RateLimit
import Wire.RateLimit.Interpreter
import Wire.Rpc
import Wire.Sem.Delay
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random.IO
import Wire.TeamCollaboratorsStore.Postgres (interpretTeamCollaboratorsStoreToPostgres)
import Wire.TeamCollaboratorsSubsystem.Interpreter

-- Effects needed by the interpretation of other effects
type GalleyEffects0 =
  '[ Input ClientState,
     Input Hasql.Pool,
     Input Env,
     Error InvalidInput,
     Error ParseException,
     Error InternalError,
     -- federation errors can be thrown by almost every endpoint, so we avoid
     -- having to declare it every single time, and simply handle it here
     Error FederationError,
     Error TeamCollaboratorsError,
     Error Hasql.UsageError,
     Error HttpError,
     Async,
     Delay,
     Fail,
     Embed IO,
     Error JSONResponse,
     Resource,
     Final IO
   ]

type GalleyEffects = Append GalleyEffects1 GalleyEffects0

-- Define some invariants for the options used
validateOptions :: Opts -> IO (Either HttpsUrl (Map Text HttpsUrl))
validateOptions o = do
  let settings' = view settings o
      optFanoutLimit = fromIntegral . fromRange $ currentFanoutLimit o
  when (settings' ^. maxConvSize > fromIntegral optFanoutLimit) $
    error "setMaxConvSize cannot be > setTruncationLimit"
  when (settings' ^. maxTeamSize < optFanoutLimit) $
    error "setMaxTeamSize cannot be < setTruncationLimit"
  case (o ^. O.federator, o ^. rabbitmq) of
    (Nothing, Just _) -> error "RabbitMQ config is specified and federator is not, please specify both or none"
    (Just _, Nothing) -> error "Federator is specified and RabbitMQ config is not, please specify both or none"
    _ -> pure ()
  let mlsFlag = settings' ^. featureFlags . to (featureDefaults @MLSConfig)
      mlsConfig = mlsFlag.config
      migrationStatus = (.status) $ settings' ^. featureFlags . to (featureDefaults @MlsMigrationConfig)
  when (migrationStatus == FeatureStatusEnabled && ProtocolMLSTag `notElem` mlsSupportedProtocols mlsConfig) $
    error "For starting MLS migration, MLS must be included in the supportedProtocol list"
  unless (mlsDefaultProtocol mlsConfig `elem` mlsSupportedProtocols mlsConfig) $
    error "The list 'settings.featureFlags.mls.supportedProtocols' must include the value in the field 'settings.featureFlags.mls.defaultProtocol'"
  let errMsg = "Either conversationCodeURI or multiIngress needs to be set."
  case (settings' ^. conversationCodeURI, settings' ^. multiIngress) of
    (Nothing, Nothing) -> error errMsg
    (Nothing, Just mi) -> pure (Right mi)
    (Just uri, Nothing) -> pure (Left uri)
    (Just _, Just _) -> error errMsg

createEnv :: Opts -> Logger -> IO Env
createEnv o l = do
  cass <- initCassandra o l
  mgr <- initHttpManager o
  h2mgr <- initHttp2Manager
  codeURIcfg <- validateOptions o
  postgres <- initPostgresPool o._postgresql o._postgresqlPassword
  Env (RequestId defRequestId) o l mgr h2mgr (o ^. O.federator) (o ^. O.brig) cass postgres
    <$> Q.new 16000
    <*> initExtEnv
    <*> maybe (pure Nothing) (fmap Just . Aws.mkEnv l mgr) (o ^. journal)
    <*> traverse loadAllMLSKeys (o ^. settings . mlsPrivateKeyPaths)
    <*> traverse (mkRabbitMqChannelMVar l (Just "galley")) (o ^. rabbitmq)
    <*> pure codeURIcfg
    <*> newRateLimitEnv (o ^. settings . passwordHashingRateLimit)

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra o l =
  initCassandraForService
    (o ^. cassandra)
    "galley"
    (o ^. discoUrl)
    Nothing
    l

initHttpManager :: Opts -> IO Manager
initHttpManager o = do
  ctx <- Ssl.context
  Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextSetDefaultVerifyPaths ctx
  newManager
    (opensslManagerSettings (pure ctx))
      { managerResponseTimeout = responseTimeoutMicro 10000000,
        managerConnCount = o ^. settings . httpPoolSize,
        managerIdleConnectionCount = 3 * (o ^. settings . httpPoolSize)
      }

initHttp2Manager :: IO Http2Manager
initHttp2Manager = do
  ctx <- Ssl.context
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextSetVerificationMode ctx $
    Ssl.VerifyPeer True True Nothing
  Ssl.contextSetDefaultVerifyPaths ctx
  http2ManagerWithSSLCtx ctx

interpretTinyLog ::
  (Member (Embed IO) r) =>
  Env ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog e = interpret $ \case
  P.Log l m -> Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

evalGalleyToIO :: Env -> Sem GalleyEffects a -> IO a
evalGalleyToIO env action = do
  r <-
    -- log IO exceptions
    runExceptT (evalGalley env action) `UnliftIO.catch` \(e :: SomeException) -> do
      Log.err (env ^. applog) $
        Log.msg ("IO Exception occurred" :: ByteString)
          . Log.field "message" (displayException e)
          . Log.field "request" (unRequestId (env ^. reqId))
      UnliftIO.throwIO e
  case r of
    -- throw any errors as IO exceptions without logging them
    Left e -> UnliftIO.throwIO e
    Right a -> pure a

toServantHandler :: Env -> Sem GalleyEffects a -> Servant.Handler a
toServantHandler env = liftIO . evalGalleyToIO env

evalGalley :: Env -> Sem GalleyEffects a -> ExceptT JSONResponse IO a
evalGalley e =
  ExceptT
    . runFinal @IO
    . resourceToIOFinal
    . runError
    . embedToFinal @IO
    . failToEmbed @IO
    . runDelay
    . asyncToIOFinal
    . mapError httpErrorToJSONResponse
    . mapError postgresUsageErrorToHttpError
    . mapError teamCollaboratorsSubsystemErrorToHttpError
    . mapError toResponse
    . mapError toResponse
    . mapError toResponse
    . mapError toResponse
    . runInputConst e
    . runInputConst (e ^. hasqlPool)
    . runInputConst (e ^. cstate)
    . mapError rateLimitExceededToHttpError
    . mapError toResponse -- DynError
    . interpretTinyLog e
    . interpretQueue (e ^. deleteQueue)
    . nowToIO
    . runInputConst (e ^. options)
    . runInputConst (toLocalUnsafe (e ^. options . settings . federationDomain) ())
    . interpretTeamFeatureSpecialContext e
    . runInputSem getAllTeamFeaturesForServer
    . interpretInternalTeamListToCassandra
    . interpretTeamListToCassandra
    . interpretLegacyConversationListToCassandra
    . interpretRemoteConversationListToCassandra
    . interpretConversationListToCassandra
    . interpretTeamMemberStoreToCassandraWithPaging lh
    . interpretTeamMemberStoreToCassandra lh
    . interpretTeamFeatureStoreToCassandra
    . interpretConversationStoreToCassandra (e ^. cstate)
    . interpretTeamStoreToCassandra lh
    . interpretTeamNotificationStoreToCassandra
    . interpretServiceStoreToCassandra
    . interpretSearchVisibilityStoreToCassandra
    . interpretLegalHoldStoreToCassandra lh
    . interpretCustomBackendStoreToCassandra
    . randomToIO
    . runHashPassword e._options._settings._passwordHashingOptions
    . interpretRateLimit e._passwordHashingRateLimitEnv
    . interpretProposalStoreToCassandra
    . interpretCodeStoreToCassandra
    . interpretClientStoreToCassandra
    . interpretTeamCollaboratorsStoreToPostgres
    . interpretFireAndForget
    . interpretBackendNotificationQueueAccess
    . interpretFederatorAccess
    . runRpcWithHttp (e ^. manager) (e ^. reqId)
    . runGundeckAPIAccess (e ^. options . gundeck)
    . interpretTeamSubsystem
    . runNotificationSubsystemGundeck (notificationSubsystemConfig e)
    . interpretTeamCollaboratorsSubsystem
    . interpretSparAccess
    . interpretBrigAccess (e ^. brig)
    . interpretExternalAccess
  where
    lh = view (options . settings . featureFlags . to npProject) e

interpretTeamFeatureSpecialContext :: Env -> Sem (Input (Maybe [TeamId], FeatureDefaults LegalholdConfig) ': r) a -> Sem r a
interpretTeamFeatureSpecialContext e =
  runInputConst
    ( e ^. options . settings . exposeInvitationURLsTeamAllowlist,
      e ^. options . settings . featureFlags . to npProject
    )
