-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Effects
  ( runBackgroundWorkerEffects,
  )
where

import Bilge qualified
import Bilge.Retry
import Cassandra (ClientState)
import Control.Monad.Catch
import Control.Retry
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Tagged (Tagged)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Galley.Types.Error (InternalError, internalErrorDescription, legalHoldServiceUnavailable)
import Hasql.Pool (UsageError)
import Hasql.Pool qualified as Hasql
import Imports
import Network.HTTP.Client qualified as Http
import Network.Wai.Utilities.JSONResponse (JSONResponse (..))
import OpenSSL.Session qualified as SSL
import Polysemy
import Polysemy.Async (Async, asyncToIOFinal)
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog qualified as P
import Ssl.Util
import System.Logger as Logger
import System.Logger qualified as Log
import URI.ByteString (uriPath)
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Error (APIError (toResponse), DynError (..))
import Wire.API.Error.Galley
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error (FederationError)
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.Collaborator (TeamCollaboratorsError)
import Wire.API.Team.Feature (AllTeamFeatures, LegalholdConfig)
import Wire.API.Team.FeatureFlags (FanoutLimit, FeatureDefaults (FeatureLegalHoldDisabledPermanently), FeatureFlags, currentFanoutLimit)
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BackendNotificationQueueAccess.RabbitMq qualified as BackendNotificationQueueAccess
import Wire.BackgroundWorker.Env (Env (..))
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess.Rpc
import Wire.ClientSubsystem.Error (ClientError)
import Wire.CodeStore (CodeStore)
import Wire.CodeStore.Cassandra (interpretCodeStoreToCassandra)
import Wire.CodeStore.DualWrite (interpretCodeStoreToCassandraAndPostgres)
import Wire.CodeStore.Postgres (interpretCodeStoreToPostgres)
import Wire.ConversationStore (ConversationStore, MLSCommitLockStore)
import Wire.ConversationStore.Cassandra (MigrationError (..), interpretConversationStoreByMigration, interpretMLSCommitLockStoreToCassandra)
import Wire.ConversationSubsystem (ConversationSubsystem)
import Wire.ConversationSubsystem.Interpreter (ConversationSubsystemError, GroupInfoCheckEnabled (..), IntraListing (..), interpretConversationSubsystem)
import Wire.ExternalAccess (ExternalAccess)
import Wire.ExternalAccess.External
import Wire.FeaturesConfigSubsystem (FeaturesConfigSubsystem, getAllTeamFeaturesForServer)
import Wire.FeaturesConfigSubsystem.Interpreter (runFeaturesConfigSubsystem)
import Wire.FeaturesConfigSubsystem.Types (ExposeInvitationURLsAllowlist (..))
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationAPIAccess.Interpreter (FederationAPIAccessConfig (..), interpretFederationAPIAccess)
import Wire.FederationSubsystem (FederationSubsystem)
import Wire.FederationSubsystem.Interpreter (runFederationSubsystem)
import Wire.FireAndForget (FireAndForget, interpretFireAndForget)
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.GundeckAPIAccess
import Wire.HashPassword (HashPassword)
import Wire.HashPassword.Interpreter (runHashPassword)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.LegalHoldStore.Cassandra (interpretLegalHoldStoreToCassandra)
import Wire.LegalHoldStore.Env (LegalHoldEnv (..))
import Wire.NotificationSubsystem (NotificationSubsystem)
import Wire.NotificationSubsystem.Interpreter
import Wire.Options.Galley (GuestLinkTTLSeconds)
import Wire.ParseException
import Wire.PostgresMigrationOpts
import Wire.ProposalStore (ProposalStore)
import Wire.ProposalStore.Cassandra (interpretProposalStoreToCassandra)
import Wire.RateLimit (RateLimit, RateLimitExceeded)
import Wire.RateLimit.Interpreter (interpretRateLimit)
import Wire.Rpc
import Wire.RpcException (RpcException)
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe))
import Wire.Sem.Concurrency.IO (unsafelyPerformConcurrency)
import Wire.Sem.Delay (Delay, runDelay)
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)
import Wire.ServiceStore (ServiceStore)
import Wire.ServiceStore.Cassandra (interpretServiceStoreToCassandra)
import Wire.SparAPIAccess (SparAPIAccess)
import Wire.SparAPIAccess.Rpc (interpretSparAPIAccessToRpc)
import Wire.TeamCollaboratorsStore (TeamCollaboratorsStore)
import Wire.TeamCollaboratorsStore.Postgres (interpretTeamCollaboratorsStoreToPostgres)
import Wire.TeamCollaboratorsSubsystem (TeamCollaboratorsSubsystem)
import Wire.TeamCollaboratorsSubsystem.Interpreter (interpretTeamCollaboratorsSubsystem)
import Wire.TeamFeatureStore (TeamFeatureStore)
import Wire.TeamFeatureStore.Cassandra (interpretTeamFeatureStoreToCassandra)
import Wire.TeamFeatureStore.Error (TeamFeatureStoreError)
import Wire.TeamJournal (TeamJournal)
import Wire.TeamJournal.Aws (interpretTeamJournal)
import Wire.TeamStore (TeamStore)
import Wire.TeamStore.Cassandra (interpretTeamStoreToCassandra)
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem.Interpreter (TeamSubsystemConfig (..), interpretTeamSubsystem)
import Wire.UserClientIndexStore (UserClientIndexStore)
import Wire.UserClientIndexStore.Cassandra
import Wire.UserGroupStore (UserGroupStore)
import Wire.UserGroupStore.Postgres (interpretUserGroupStoreToPostgres)

makeVerifiedRequestWithManagerIO ::
  Logger ->
  Http.Manager ->
  ([Fingerprint Rsa] -> SSL.SSL -> IO ()) ->
  Fingerprint Rsa ->
  HttpsUrl ->
  (Http.Request -> Http.Request) ->
  IO (Http.Response LBS.ByteString)
makeVerifiedRequestWithManagerIO logger mgr verifyFingerprints fpr (HttpsUrl url) reqBuilder = do
  let verified = verifyFingerprints [fpr]
  extHandleAll (errHandler logger) $ do
    recovering legalHoldRetryPolicy httpHandlers $
      const $
        withVerifiedSslConnection verified mgr (reqBuilderMods . reqBuilder) $
          \req ->
            Http.httpLbs req mgr
  where
    reqBuilderMods =
      maybe id Bilge.host (Bilge.extHost url)
        . Bilge.port (fromMaybe 443 (Bilge.extPort url))
        . Bilge.secure
        . prependPath (uriPath url)
    errHandler logger' e = do
      Logger.info logger' $ Log.msg ("error making request to legalhold service: " <> displayException e)
      throwM (legalHoldServiceUnavailable e)
    prependPath :: BS.ByteString -> Http.Request -> Http.Request
    prependPath pth req = req {Http.path = pth `BS.append` Http.path req}
    legalHoldRetryPolicy :: RetryPolicy
    legalHoldRetryPolicy = limitRetries 3 <> exponentialBackoff 100000
    extHandleAll :: (MonadCatch m) => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma =
      catches
        ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex,
          Handler $ \(ex :: SomeException) -> f ex
        ]

makeVerifiedRequestIO :: Logger -> ExtEnv -> Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LBS.ByteString)
makeVerifiedRequestIO logger extEnv fpr url reqBuilder = do
  let (mgr, verifyFingerprints) = extGetManager extEnv
  makeVerifiedRequestWithManagerIO logger mgr verifyFingerprints fpr url reqBuilder

makeVerifiedRequestFreshManagerIO :: Logger -> Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LBS.ByteString)
makeVerifiedRequestFreshManagerIO logger fpr url reqBuilder = do
  let disableTlsV1 = True
  ExtEnv (mgr, verifyFingerprints) <- initExtEnv disableTlsV1
  makeVerifiedRequestWithManagerIO logger mgr verifyFingerprints fpr url reqBuilder

type BackgroundWorkerEffects =
  '[ ConversationSubsystem,
     TeamCollaboratorsSubsystem,
     Input AllTeamFeatures,
     FeaturesConfigSubsystem,
     FederationSubsystem,
     TeamSubsystem,
     FederationAPIAccess FederatorClient,
     NotificationSubsystem,
     SparAPIAccess,
     ExternalAccess,
     RateLimit,
     HashPassword,
     Input FeatureFlags,
     Input (Maybe (MLSKeysByPurpose MLSPrivateKeys)),
     Input ConversationSubsystemConfig,
     GalleyAPIAccess,
     BrigAPIAccess,
     GundeckAPIAccess,
     Rpc,
     CodeStore,
     BackendNotificationQueueAccess,
     FireAndForget,
     Random,
     Now,
     TeamJournal,
     LegalHoldStore,
     TeamCollaboratorsStore,
     TeamStore,
     ConversationStore,
     UserClientIndexStore,
     TeamFeatureStore,
     UserGroupStore,
     ServiceStore,
     ProposalStore,
     MLSCommitLockStore,
     Input FanoutLimit,
     Input (Maybe GuestLinkTTLSeconds),
     Input (Maybe GroupInfoCheckEnabled),
     Input IntraListing,
     Input (Either HttpsUrl (Map Text HttpsUrl)),
     Input ExposeInvitationURLsAllowlist,
     Input LegalHoldEnv,
     Input ClientState,
     Input (FeatureDefaults LegalholdConfig),
     Input (Local ()),
     Input Hasql.Pool,
     P.TinyLog,
     Error RateLimitExceeded,
     Error UnreachableBackendsLegacy,
     Error NonFederatingBackends,
     Error (Tagged AccessDenied ()),
     Error (Tagged TeamMemberNotFound ()),
     Error (Tagged TeamNotFound ()),
     Error (Tagged ConvAccessDenied ()),
     Error (Tagged NotATeamMember ()),
     Error TeamFeatureStoreError,
     Error TeamCollaboratorsError,
     Error UnreachableBackends,
     Error InternalError,
     Error MigrationError,
     Error ParseException,
     Error UsageError,
     Error FederationError,
     Error ClientError,
     Error RpcException,
     Error ConversationSubsystemError,
     Error JSONResponse,
     Error DynError,
     Error Text,
     Resource,
     Delay,
     Race,
     Async,
     Embed IO,
     Concurrency Unsafe,
     Final IO
   ]

runBackgroundWorkerEffects ::
  Env ->
  ExtEnv ->
  RequestId ->
  Maybe JobId ->
  Sem BackgroundWorkerEffects a ->
  IO (Either Text a)
runBackgroundWorkerEffects env extEnv requestId mJobId =
  runFinal @IO
    . unsafelyPerformConcurrency @_ @'Unsafe
    . embedToFinal @IO
    . asyncToIOFinal
    . interpretRace
    . runDelay
    . resourceToIOFinal
    . runError
    . mapError @DynError (.eMessage)
    . mapError @JSONResponse (T.pack . show . (.value))
    . mapError @ConversationSubsystemError toResponse
    . mapError @RpcException (T.pack . displayException)
    . mapError @ClientError (T.pack . displayException)
    . mapError @FederationError (T.pack . displayException)
    . mapError @UsageError (T.pack . show)
    . mapError @ParseException (T.pack . displayException)
    . mapError @MigrationError (T.pack . show)
    . mapError @InternalError (TL.toStrict . internalErrorDescription)
    . mapError @UnreachableBackends (T.pack . show)
    . mapError @TeamCollaboratorsError (const ("Team collaborators error" :: Text))
    . mapError @TeamFeatureStoreError (const ("Team feature store error" :: Text))
    . mapError @(Tagged 'NotATeamMember ()) (const ("Not a team member" :: Text))
    . mapError @(Tagged 'ConvAccessDenied ()) (const ("Conversation access denied" :: Text))
    . mapError @(Tagged 'TeamNotFound ()) (const ("Team not found" :: Text))
    . mapError @(Tagged 'TeamMemberNotFound ()) (const ("Team member not found" :: Text))
    . mapError @(Tagged 'AccessDenied ()) (const ("Access denied" :: Text))
    . mapError @NonFederatingBackends (const ("Non federating backends" :: Text))
    . mapError @UnreachableBackendsLegacy (const ("Unreachable backends legacy" :: Text))
    . mapError @RateLimitExceeded (const ("Rate limit exceeded" :: Text))
    . interpretTinyLog
    . runInputConst @Hasql.Pool env.hasqlPool
    . runInputConst @(Local ()) (toLocalUnsafe env.federationDomain ())
    . runInputConst @(FeatureDefaults LegalholdConfig) FeatureLegalHoldDisabledPermanently
    . runInputConst @ClientState env.cassandraGalley
    . runInputConst @LegalHoldEnv legalHoldEnv
    . runInputConst @ExposeInvitationURLsAllowlist (ExposeInvitationURLsAllowlist $ fromMaybe [] env.exposeInvitationURLsTeamAllowlist)
    . runInputConst @(Either HttpsUrl (Map Text HttpsUrl)) env.convCodeURI
    . runInputConst @IntraListing (IntraListing env.intraListing)
    . runInputConst @(Maybe GroupInfoCheckEnabled) (GroupInfoCheckEnabled <$> env.checkGroupInfo)
    . runInputConst @(Maybe GuestLinkTTLSeconds) env.guestLinkTTLSeconds
    . runInputConst @FanoutLimit (currentFanoutLimit env.maxTeamSize env.maxFanoutSize)
    . interpretMLSCommitLockStoreToCassandra env.cassandraGalley
    . interpretProposalStoreToCassandra
    . interpretServiceStoreToCassandra env.cassandraBrig
    . interpretUserGroupStoreToPostgres
    . interpretTeamFeatureStoreToCassandra
    . interpretUserClientIndexStoreToCassandra env.cassandraGalley
    . interpretConversationStoreByMigration env.postgresMigration.conversation env.cassandraGalley
    . interpretTeamStoreToCassandra
    . interpretTeamCollaboratorsStoreToPostgres
    . interpretLegalHoldStoreToCassandra FeatureLegalHoldDisabledPermanently
    . interpretTeamJournal Nothing
    . nowToIO
    . randomToIO
    . interpretFireAndForget
    . BackendNotificationQueueAccess.interpretBackendNotificationQueueAccess (Just backendQueueEnv)
    . convCodesStoreInterpreter
    . runRpcWithHttp env.httpManager requestId
    . runGundeckAPIAccess env.gundeckEndpoint
    -- FUTUREWORK: Currently the brig access effect is needed for the interpreter of ExternalAccess.
    -- At the time of implementation the only function used from ExternalAccess is deliverAsync, which will not call brig access.
    -- However, to prevent the background worker to require HTTP access to brig, we should consider refactoring this at some point.
    . interpretBrigAccess env.brigEndpoint
    . interpretGalleyAPIAccessToRpc mempty env.galleyEndpoint
    . runInputSem getConversationSubsystemConfig
    . runInputSem @(Maybe (MLSKeysByPurpose MLSPrivateKeys)) (inputs @ConversationSubsystemConfig (.mlsKeys))
    . runInputSem getConfiguredFeatureFlags
    . runHashPassword env.passwordHashingOptions
    . interpretRateLimit env.passwordHashingRateLimitEnv
    . interpretExternalAccess extEnv
    . interpretSparAPIAccessToRpc env.sparEndpoint
    . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig requestId)
    . interpretFederationAPIAccess federationAPIAccessConfig
    . interpretTeamSubsystem teamSubsystemConfig
    . ( \m -> do
          p <- inputs @ConversationSubsystemConfig (.federationProtocols)
          runFederationSubsystem p m
      )
    . runFeaturesConfigSubsystem
    . runInputSem getAllTeamFeaturesForServer
    . interpretTeamCollaboratorsSubsystem
    . interpretConversationSubsystem
  where
    convCodesStoreInterpreter =
      case env.postgresMigration.conversationCodes of
        CassandraStorage -> interpretCodeStoreToCassandra
        MigrationToPostgresql -> interpretCodeStoreToCassandraAndPostgres
        PostgresqlStorage -> interpretCodeStoreToPostgres
    legalHoldEnv =
      let makeReq fpr url rb = makeVerifiedRequestIO env.logger extEnv fpr url rb
          makeReqFresh fpr url rb = makeVerifiedRequestFreshManagerIO env.logger fpr url rb
       in LegalHoldEnv {makeVerifiedRequest = makeReq, makeVerifiedRequestFreshManager = makeReqFresh}
    teamSubsystemConfig = TeamSubsystemConfig {concurrentDeletionEvents = 1}
    federationAPIAccessConfig =
      FederationAPIAccessConfig
        { ownDomain = env.federationDomain,
          federatorEndpoint = Just env.federatorInternal,
          http2Manager = env.http2Manager,
          requestId = requestId
        }
    getConversationSubsystemConfig ::
      (Member GalleyAPIAccess r) =>
      Sem r ConversationSubsystemConfig
    getConversationSubsystemConfig = getConversationConfig
    backendQueueEnv =
      BackendNotificationQueueAccess.Env
        { channelMVar = env.amqpBackendNotificationsChannel,
          logger = env.logger,
          local = toLocalUnsafe env.federationDomain (),
          requestId = requestId
        }
    interpretTinyLog :: (Member (Embed IO) r) => Sem (P.TinyLog ': r) a -> Sem r a
    interpretTinyLog =
      loggerToTinyLog env.logger
        . mapLogger (loggerFields .)
        . raiseUnder @P.TinyLog
    loggerFields :: Log.Msg -> Log.Msg
    loggerFields =
      case mJobId of
        Nothing -> field "request" (unRequestId requestId)
        Just jId -> field "request" (unRequestId requestId) . field "job" (idToText jId)
