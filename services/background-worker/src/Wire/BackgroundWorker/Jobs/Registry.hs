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

module Wire.BackgroundWorker.Jobs.Registry
  ( dispatchJob,
  )
where

import Bilge qualified
import Bilge.Retry
import Cassandra (ClientState)
import Control.Monad.Catch
import Control.Retry
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LC8
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
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource (resourceToIOFinal)
import Polysemy.TinyLog qualified as P
import Ssl.Util
import System.Logger as Logger
import System.Logger.Class qualified as Log
import URI.ByteString (uriPath)
import Wire.API.BackgroundJobs (Job (..))
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Error (APIError (toResponse), DynError (..))
import Wire.API.Error.Galley
import Wire.API.Federation.Error (FederationError)
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Team.Collaborator (TeamCollaboratorsError)
import Wire.API.Team.Feature (LegalholdConfig)
import Wire.API.Team.FeatureFlags (FanoutLimit, FeatureDefaults (FeatureLegalHoldDisabledPermanently), currentFanoutLimit)
import Wire.BackendNotificationQueueAccess.RabbitMq qualified as BackendNotificationQueueAccess
import Wire.BackgroundJobsPublisher.RabbitMQ (interpretBackgroundJobsPublisherRabbitMQ)
import Wire.BackgroundJobsRunner (runJob)
import Wire.BackgroundJobsRunner.Interpreter hiding (runJob)
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.BackgroundWorker.Options (Settings (..))
import Wire.BrigAPIAccess.Rpc
import Wire.ClientSubsystem.Error (ClientError)
import Wire.CodeStore.Cassandra (interpretCodeStoreToCassandra)
import Wire.CodeStore.DualWrite (interpretCodeStoreToCassandraAndPostgres)
import Wire.CodeStore.Postgres (interpretCodeStoreToPostgres)
import Wire.ConversationStore.Cassandra
import Wire.ConversationStore.Postgres (interpretConversationStoreToPostgres)
import Wire.ConversationSubsystem.Interpreter (ConversationSubsystemError, GroupInfoCheckEnabled (..), GuestLinkTTLSeconds (..), IntraListing (..), interpretConversationSubsystem)
import Wire.ExternalAccess.External
import Wire.FeaturesConfigSubsystem (getAllTeamFeaturesForServer)
import Wire.FeaturesConfigSubsystem.Interpreter (runFeaturesConfigSubsystem)
import Wire.FeaturesConfigSubsystem.Types (ExposeInvitationURLsAllowlist (..))
import Wire.FederationAPIAccess.Interpreter (FederationAPIAccessConfig (..), interpretFederationAPIAccess)
import Wire.FederationSubsystem.Interpreter (runFederationSubsystem)
import Wire.FireAndForget (interpretFireAndForget)
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.GundeckAPIAccess
import Wire.HashPassword.Interpreter (runHashPassword)
import Wire.LegalHoldStore.Cassandra (interpretLegalHoldStoreToCassandra)
import Wire.LegalHoldStore.Env (LegalHoldEnv (..))
import Wire.NotificationSubsystem.Interpreter
import Wire.ParseException
import Wire.PostgresMigrationOpts
import Wire.ProposalStore.Cassandra (interpretProposalStoreToCassandra)
import Wire.RateLimit (RateLimitExceeded)
import Wire.RateLimit.Interpreter (interpretRateLimit)
import Wire.Rpc
import Wire.Sem.Concurrency (ConcurrencySafety (Unsafe))
import Wire.Sem.Concurrency.IO (unsafelyPerformConcurrency)
import Wire.Sem.Delay (runDelay)
import Wire.Sem.Logger (mapLogger)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random.IO (randomToIO)
import Wire.ServiceStore.Cassandra (interpretServiceStoreToCassandra)
import Wire.SparAPIAccess.Rpc (interpretSparAPIAccessToRpc)
import Wire.TeamCollaboratorsStore.Postgres (interpretTeamCollaboratorsStoreToPostgres)
import Wire.TeamCollaboratorsSubsystem.Interpreter (interpretTeamCollaboratorsSubsystem)
import Wire.TeamFeatureStore.Cassandra (interpretTeamFeatureStoreToCassandra)
import Wire.TeamFeatureStore.Error (TeamFeatureStoreError)
import Wire.TeamJournal.Aws (interpretTeamJournal)
import Wire.TeamStore.Cassandra (interpretTeamStoreToCassandra)
import Wire.TeamSubsystem.Interpreter (TeamSubsystemConfig (..), interpretTeamSubsystem)
import Wire.UserClientIndexStore.Cassandra
import Wire.UserGroupStore.Postgres (interpretUserGroupStoreToPostgres)

-- Helper functions for LegalHoldEnv
-- Adapted from Galley.External.LegalHoldService.Internal
makeVerifiedRequestWithManagerIO ::
  Logger ->
  Http.Manager ->
  ([Fingerprint Rsa] -> SSL.SSL -> IO ()) ->
  Fingerprint Rsa ->
  HttpsUrl ->
  (Http.Request -> Http.Request) ->
  IO (Http.Response LC8.ByteString)
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
    prependPath pth req = req {Http.path = pth `BS.append` Http.path req} -- Modified to use BS.append
    -- (</>) from System.FilePath, but here we just need to append.
    -- Assuming a simple append is sufficient for URI path segments for this context.
    legalHoldRetryPolicy :: RetryPolicy
    legalHoldRetryPolicy = limitRetries 3 <> exponentialBackoff 100000
    extHandleAll :: (MonadCatch m) => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma =
      catches
        ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex,
          Handler $ \(ex :: SomeException) -> f ex
        ]

makeVerifiedRequestIO :: Logger -> ExtEnv -> Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LC8.ByteString)
makeVerifiedRequestIO logger extEnv fpr url reqBuilder = do
  let (mgr, verifyFingerprints) = extGetManager extEnv
  makeVerifiedRequestWithManagerIO logger mgr verifyFingerprints fpr url reqBuilder

makeVerifiedRequestFreshManagerIO :: Logger -> Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LC8.ByteString)
makeVerifiedRequestFreshManagerIO logger fpr url reqBuilder = do
  let disableTlsV1 = True
  ExtEnv (mgr, verifyFingerprints) <- initExtEnv disableTlsV1
  makeVerifiedRequestWithManagerIO logger mgr verifyFingerprints fpr url reqBuilder

dispatchJob :: Job -> AppT IO (Either Text ())
dispatchJob job = do
  env <- ask @Env
  let disableTlsV1 = True
  extEnv <- liftIO (initExtEnv disableTlsV1)
  liftIO $ runInterpreters env extEnv $ runJob job
  where
    convStoreInterpreter env =
      case env.postgresMigration.conversation of
        CassandraStorage -> interpretConversationStoreToCassandra env.cassandraGalley
        MigrationToPostgresql -> interpretConversationStoreToCassandraAndPostgres env.cassandraGalley
        PostgresqlStorage -> interpretConversationStoreToPostgres
    runInterpreters env extEnv = do
      let federationAPIAccessConfig =
            FederationAPIAccessConfig
              { ownDomain = env.federationDomain,
                federatorEndpoint = Just env.federatorInternal,
                http2Manager = env.http2Manager,
                requestId = job.requestId
              }
          teamSubsystemConfig = TeamSubsystemConfig {concurrentDeletionEvents = 1}
          legalHoldEnv =
            let makeReq fpr url rb = makeVerifiedRequestIO env.logger extEnv fpr url rb
                makeReqFresh fpr url rb = makeVerifiedRequestFreshManagerIO env.logger fpr url rb
             in LegalHoldEnv {makeVerifiedRequest = makeReq, makeVerifiedRequestFreshManager = makeReqFresh}
          convCodesStoreInterpreter =
            case env.postgresMigration.conversationCodes of
              CassandraStorage -> interpretCodeStoreToCassandra
              MigrationToPostgresql -> interpretCodeStoreToCassandraAndPostgres
              PostgresqlStorage -> interpretCodeStoreToPostgres
      runFinal @IO
        . unsafelyPerformConcurrency @_ @'Unsafe
        . embedToFinal @IO
        . asyncToIOFinal
        . interpretRace
        . runDelay
        . resourceToIOFinal
        . runError
        . mapError @DynError (T.pack . show . (.eMessage))
        . mapError @JSONResponse (T.pack . show . (.value))
        . mapError @ConversationSubsystemError toResponse
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
        . interpretTinyLog env job.requestId job.jobId
        . runInputConst @Hasql.Pool env.hasqlPool
        . runInputConst @(Local ()) (toLocalUnsafe env.federationDomain ())
        . runInputConst @(FeatureDefaults LegalholdConfig) FeatureLegalHoldDisabledPermanently
        . runInputConst @ClientState env.cassandraGalley
        . runInputConst @LegalHoldEnv legalHoldEnv
        . runInputConst @ExposeInvitationURLsAllowlist (ExposeInvitationURLsAllowlist $ fromMaybe [] env.settings.exposeInvitationURLsTeamAllowlist)
        . runInputConst @(Either HttpsUrl (Map Text HttpsUrl)) env.convCodeURI
        . runInputConst @IntraListing (IntraListing env.settings.intraListing)
        . runInputConst @(Maybe GroupInfoCheckEnabled) (GroupInfoCheckEnabled <$> env.settings.checkGroupInfo)
        . runInputConst @(Maybe GuestLinkTTLSeconds) env.settings.guestLinkTTLSeconds
        . runInputConst @FanoutLimit (currentFanoutLimit env.settings.maxTeamSize env.settings.maxFanoutSize)
        . interpretMLSCommitLockStoreToCassandra env.cassandraGalley
        . interpretProposalStoreToCassandra
        . interpretServiceStoreToCassandra env.cassandraBrig
        . interpretUserGroupStoreToPostgres
        . interpretTeamFeatureStoreToCassandra
        . interpretUserClientIndexStoreToCassandra env.cassandraGalley
        . convStoreInterpreter env
        . interpretTeamStoreToCassandra
        . interpretTeamCollaboratorsStoreToPostgres
        . interpretLegalHoldStoreToCassandra FeatureLegalHoldDisabledPermanently
        . interpretTeamJournal Nothing
        . interpretBackgroundJobsPublisherRabbitMQ job.requestId env.amqpJobsPublisherChannel
        . nowToIO
        . randomToIO
        . interpretFireAndForget
        . BackendNotificationQueueAccess.interpretBackendNotificationQueueAccess (Just $ backendQueueEnv env)
        . runRpcWithHttp env.httpManager job.requestId
        . runGundeckAPIAccess env.gundeckEndpoint
        -- FUTUREWORK: Currently the brig access effect is needed for the interpreter of ExternalAccess.
        -- At the time of implementation the only function used from ExternalAccess is deliverAsync, which will not call brig access.
        -- However, to prevent the background worker to require HTTP access to brig, we should consider refactoring this at some point.
        . interpretBrigAccess env.brigEndpoint
        . interpretGalleyAPIAccessToRpc mempty env.galleyEndpoint
        . runInputSem getConversationSubsystemConfig
        . runInputSem @(Maybe (MLSKeysByPurpose MLSPrivateKeys)) (inputs @ConversationSubsystemConfig (.mlsKeys))
        . runInputSem getConfiguredFeatureFlags
        . runHashPassword env.settings.passwordHashingOptions
        . interpretRateLimit env.passwordHashingRateLimitEnv
        . convCodesStoreInterpreter
        . interpretExternalAccess extEnv
        . interpretSparAPIAccessToRpc env.sparEndpoint
        . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig job.requestId)
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
        . interpretBackgroundJobsRunner

    getConversationSubsystemConfig ::
      (Member GalleyAPIAccess r) =>
      Sem r ConversationSubsystemConfig
    getConversationSubsystemConfig = getConversationConfig

    backendQueueEnv :: Env -> BackendNotificationQueueAccess.Env
    backendQueueEnv env =
      BackendNotificationQueueAccess.Env
        { channelMVar = env.amqpBackendNotificationsChannel,
          logger = env.logger,
          local = toLocalUnsafe env.federationDomain (),
          requestId = job.requestId
        }

interpretTinyLog ::
  (Member (Embed IO) r) =>
  Env ->
  RequestId ->
  JobId ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog e reqId jobId =
  loggerToTinyLog e.logger
    . mapLogger ((field "request" (unRequestId reqId) . field "job" (idToText jobId)) .)
    . raiseUnder @P.TinyLog
