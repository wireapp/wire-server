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
    monitor,
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
import Cassandra qualified as C
import Cassandra.Settings qualified as C
import Control.Error hiding (err)
import Control.Lens hiding ((.=))
import Data.Default (def)
import Data.List.NonEmpty qualified as NE
import Data.Metrics.Middleware
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Text (unpack)
import Data.Time.Clock
import Galley.API.Error
import Galley.Aws qualified as Aws
import Galley.Cassandra.Client
import Galley.Cassandra.Code
import Galley.Cassandra.Conversation
import Galley.Cassandra.Conversation.Members
import Galley.Cassandra.ConversationList
import Galley.Cassandra.CustomBackend
import Galley.Cassandra.LegalHold
import Galley.Cassandra.Proposal
import Galley.Cassandra.SearchVisibility
import Galley.Cassandra.Services
import Galley.Cassandra.Team
import Galley.Cassandra.TeamFeatures
import Galley.Cassandra.TeamNotifications
import Galley.Effects
import Galley.Effects.FireAndForget (interpretFireAndForget)
import Galley.Effects.WaiRoutes.IO
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
import Galley.Types.Teams qualified as Teams
import HTTP2.Client.Manager (Http2Manager, http2ManagerWithSSLCtx)
import Imports hiding (forkIO)
import Network.AMQP.Extended (mkRabbitMqChannelMVar)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai.Utilities.JSONResponse
import OpenSSL.Session as Ssl
import Polysemy
import Polysemy.Error
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
import Util.Options
import Wire.API.Error
import Wire.API.Federation.Error
import Wire.Sem.Logger qualified

-- Effects needed by the interpretation of other effects
type GalleyEffects0 =
  '[ Input ClientState,
     Input Env,
     Error InvalidInput,
     Error InternalError,
     -- federation errors can be thrown by almost every endpoint, so we avoid
     -- having to declare it every single time, and simply handle it here
     Error FederationError,
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
  let errMsg = "Either conversationCodeURI or multiIngress needs to be set."
  case (settings' ^. conversationCodeURI, settings' ^. multiIngress) of
    (Nothing, Nothing) -> error errMsg
    (Nothing, Just mi) -> pure (Right mi)
    (Just uri, Nothing) -> pure (Left uri)
    (Just _, Just _) -> error errMsg

createEnv :: Metrics -> Opts -> Logger -> IO Env
createEnv m o l = do
  cass <- initCassandra o l
  mgr <- initHttpManager o
  h2mgr <- initHttp2Manager
  codeURIcfg <- validateOptions o
  Env def m o l mgr h2mgr (o ^. O.federator) (o ^. O.brig) cass
    <$> Q.new 16000
    <*> initExtEnv
    <*> maybe (pure Nothing) (fmap Just . Aws.mkEnv l mgr) (o ^. journal)
    <*> loadAllMLSKeys (fold (o ^. settings . mlsPrivateKeyPaths))
    <*> traverse (mkRabbitMqChannelMVar l) (o ^. rabbitmq)
    <*> pure codeURIcfg

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra o l = do
  c <-
    maybe
      (C.initialContactsPlain (o ^. cassandra . endpoint . host))
      (C.initialContactsDisco "cassandra_galley" . unpack)
      (o ^. discoUrl)
  C.init
    . C.setLogger (C.mkLogger (Logger.clone (Just "cassandra.galley") l))
    . C.setContacts (NE.head c) (NE.tail c)
    . C.setPortNumber (fromIntegral $ o ^. cassandra . endpoint . port)
    . C.setKeyspace (Keyspace $ o ^. cassandra . keyspace)
    . C.setMaxConnections 4
    . C.setMaxStreams 128
    . C.setPoolStripes 4
    . C.setSendTimeout 3
    . C.setResponseTimeout 10
    . C.setProtocolVersion C.V4
    . C.setPolicy (C.dcFilterPolicyIfConfigured l (o ^. cassandra . filterNodesByDatacentre))
    $ C.defSettings

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
  Member (Embed IO) r =>
  Env ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog e = interpret $ \case
  P.Log l m -> Logger.log (e ^. applog) (Wire.Sem.Logger.toLevel l) (reqIdMsg (e ^. reqId) . m)

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
    . mapError toResponse
    . mapError toResponse
    . mapError toResponse
    . runInputConst e
    . runInputConst (e ^. cstate)
    . mapError toResponse -- DynError
    . interpretTinyLog e
    . interpretQueue (e ^. deleteQueue)
    . runInputSem (embed getCurrentTime) -- FUTUREWORK: could we take the time only once instead?
    . interpretWaiRoutes
    . runInputConst (e ^. options)
    . runInputConst (toLocalUnsafe (e ^. options . settings . federationDomain) ())
    . interpretInternalTeamListToCassandra
    . interpretTeamListToCassandra
    . interpretLegacyConversationListToCassandra
    . interpretRemoteConversationListToCassandra
    . interpretConversationListToCassandra
    . interpretTeamMemberStoreToCassandraWithPaging lh
    . interpretTeamMemberStoreToCassandra lh
    . interpretTeamStoreToCassandra lh
    . interpretTeamNotificationStoreToCassandra
    . interpretTeamFeatureStoreToCassandra
    . interpretServiceStoreToCassandra
    . interpretSearchVisibilityStoreToCassandra
    . interpretMemberStoreToCassandra
    . interpretLegalHoldStoreToCassandra lh
    . interpretCustomBackendStoreToCassandra
    . interpretConversationStoreToCassandra
    . interpretProposalStoreToCassandra
    . interpretCodeStoreToCassandra
    . interpretClientStoreToCassandra
    . interpretFireAndForget
    . interpretBotAccess
    . interpretBackendNotificationQueueAccess
    . interpretFederatorAccess
    . interpretExternalAccess
    . interpretGundeckAccess
    . interpretDefederationNotifications
    . interpretSparAccess
    . interpretBrigAccess
  where
    lh = view (options . settings . featureFlags . Teams.flagLegalHold) e
