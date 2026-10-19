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

    -- * Running Galley effects
    GalleyEffects,
    evalGalleyToIO,
    ask,
    DeleteItem (..),
    toServantHandler,
  )
where

import Amazonka qualified
import Amazonka.SES qualified as SES
import Arbiter.Core qualified as ArbiterCore
import Bilge hiding (Request, header, host, options, port, statusCode, statusMessage)
import Cassandra hiding (Set)
import Cassandra.Options qualified
import Cassandra.Util (initCassandraForService)
import Control.Error hiding (err)
import Control.Lens hiding ((.=))
import Data.ByteString.Builder (toLazyByteString)
import Data.Default (def)
import Data.Domain (Domain)
import Data.Id
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Text qualified as Text
import Data.Time.Clock (secondsToDiffTime)
import Galley.Env
import Galley.External.LegalHoldService.Internal qualified as LHInternal
import Galley.Monad (runApp)
import Galley.Types.Error
import HTTP2.Client.Manager (Http2Manager, http2ManagerWithSSLCtx)
import Hasql.Pool qualified as Hasql
import Hasql.Pool.Extended (initPostgresPool)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports hiding (forkIO)
import Network.AMQP.Extended (mkRabbitMqChannelMVar)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.Wai.Utilities.JSONResponse
import OpenSSL.Session as Ssl
import Polysemy
import Polysemy.Async
import Polysemy.Async qualified as P
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Resource
import Polysemy.TinyLog (TinyLog, logErrors)
import Polysemy.TinyLog qualified as P
import Servant qualified
import Ssl.Util
import System.Logger qualified as Log
import System.Logger.Class (Logger)
import System.Logger.Extended qualified as Logger
import UnliftIO.Exception qualified as UnliftIO
import Util.Options (awsHost, awsPort, awsSecure, initCredentials)
import Wire.API.Conversation.Config (ConversationSubsystemConfig (..))
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley (GalleyError (..), MeetingError, NonFederatingBackends, OperationDenied, UnreachableBackends)
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import Wire.API.Federation.Version qualified as Federation
import Wire.API.MLS.Keys (MLSKeysByPurpose, MLSPrivateKeys)
import Wire.API.Meeting (defaultLegacyTimeZone, parseTimeZone)
import Wire.API.Team.Collaborator
import Wire.API.Team.Feature
import Wire.API.Team.FeatureFlags
import Wire.AWS qualified as Aws
import Wire.BackendNotificationQueueAccess (BackendNotificationQueueAccess)
import Wire.BackendNotificationQueueAccess.RabbitMq qualified as BackendNotificationQueueAccess
import Wire.BoundedQueue (BoundedQueue)
import Wire.BoundedQueue.STM
import Wire.BoundedQueue.STM qualified as Q
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess.Rpc
import Wire.CodeStore (CodeStore)
import Wire.CodeStore.Cassandra
import Wire.CodeStore.DualWrite
import Wire.CodeStore.Postgres
import Wire.ConversationStore (ConversationStore, MLSCommitLockStore)
import Wire.ConversationStore.Cassandra (MigrationError (..), interpretConversationStoreByMigration, interpretMLSCommitLockStoreToCassandra)
import Wire.ConversationSubsystem
import Wire.ConversationSubsystem.Interpreter (ConversationSubsystemError, GroupInfoCheckEnabled (..), IntraListing (IntraListing), interpretConversationSubsystem)
import Wire.CustomBackendStore
import Wire.CustomBackendStore.Cassandra
import Wire.EmailSending (EmailSending (..), sendMail)
import Wire.EmailSending qualified as EmailSending
import Wire.EmailSending.Options qualified as EmailOpt
import Wire.EmailSending.SES (EmailSendingAWSError, emailViaSESInterpreter)
import Wire.EmailSending.SMTP qualified as SMTP
import Wire.EmailSubsystem (EmailSubsystem, discardEmailSubsystem)
import Wire.EmailSubsystem.Interpreter (meetingEmailSubsystemInterpreter)
import Wire.EmailSubsystem.Template (loadMeetingTemplates)
import Wire.Error
import Wire.ExternalAccess (ExternalAccess)
import Wire.ExternalAccess.External
import Wire.FeaturesConfigSubsystem
import Wire.FeaturesConfigSubsystem.Interpreter
import Wire.FeaturesConfigSubsystem.Types (ExposeInvitationURLsAllowlist (..))
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationAPIAccess.Interpreter
import Wire.FederationSubsystem
import Wire.FederationSubsystem.Interpreter (runFederationSubsystem)
import Wire.FireAndForget
import Wire.GundeckAPIAccess (GundeckAPIAccess, runGundeckAPIAccess)
import Wire.HashPassword
import Wire.HashPassword.Interpreter
import Wire.JobSubsystem (JobSubsystem, JobSubsystemConfig (..))
import Wire.JobSubsystem.Interpreter (interpretJobSubsystem)
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.LegalHoldStore.Cassandra (interpretLegalHoldStoreToCassandra)
import Wire.LegalHoldStore.Env (LegalHoldEnv (..))
import Wire.ListItems (ListItems)
import Wire.ListItems.Team.Cassandra
  ( interpretInternalTeamListToCassandra,
    interpretTeamListToCassandra,
  )
import Wire.MeetingNotifier (MeetingNotifier)
import Wire.MeetingNotifier.Interpreter (interpretMeetingNotifier)
import Wire.MeetingsStore (MeetingsStore)
import Wire.MeetingsStore.Postgres (interpretMeetingsStoreToPostgres)
import Wire.MeetingsSubsystem (MeetingsSubsystem)
import Wire.MeetingsSubsystem.Interpreter qualified as Meeting
import Wire.MigrationLock
import Wire.NotificationSubsystem (NotificationSubsystem)
import Wire.NotificationSubsystem.Interpreter (runNotificationSubsystemGundeck)
import Wire.Options.Galley hiding (brig, endpoint, federator)
import Wire.Options.Galley qualified as O
import Wire.Options.Keys
import Wire.ParseException
import Wire.ProposalStore (ProposalStore)
import Wire.ProposalStore.Cassandra
import Wire.RateLimit
import Wire.RateLimit.Interpreter
import Wire.Rpc
import Wire.RpcException
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Delay
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Paging.Cassandra
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO
import Wire.ServiceStore (ServiceStore)
import Wire.ServiceStore.Cassandra (interpretServiceStoreToCassandra)
import Wire.SparAPIAccess (SparAPIAccess)
import Wire.SparAPIAccess.Rpc
import Wire.TeamCollaboratorsStore (TeamCollaboratorsStore)
import Wire.TeamCollaboratorsStore.Postgres (interpretTeamCollaboratorsStoreToPostgres)
import Wire.TeamCollaboratorsSubsystem (TeamCollaboratorsSubsystem)
import Wire.TeamCollaboratorsSubsystem.Interpreter
import Wire.TeamFeatureStore (TeamFeatureStore)
import Wire.TeamFeatureStore.Cassandra
import Wire.TeamFeatureStore.Error (TeamFeatureStoreError (..))
import Wire.TeamFeatureStore.Migrating
import Wire.TeamFeatureStore.Postgres
import Wire.TeamJournal (TeamJournal)
import Wire.TeamJournal.Aws
import Wire.TeamMemberStore (TeamMemberStore)
import Wire.TeamMemberStore.Cassandra
  ( interpretTeamMemberStoreToCassandra,
    interpretTeamMemberStoreToCassandraWithPaging,
  )
import Wire.TeamNotificationStore (TeamNotificationStore)
import Wire.TeamNotificationStore.Cassandra (interpretTeamNotificationStoreToCassandra)
import Wire.TeamStore (TeamStore)
import Wire.TeamStore.Cassandra (interpretTeamStoreToCassandra)
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem.Interpreter
import Wire.UserClientIndexStore (UserClientIndexStore)
import Wire.UserClientIndexStore.Cassandra (interpretUserClientIndexStoreToCassandra)
import Wire.UserGroupStore (UserGroupStore)
import Wire.UserGroupStore.Postgres (interpretUserGroupStoreToPostgres)

type GalleyEffects =
  '[ MeetingsSubsystem,
     EmailSubsystem,
     EmailSending,
     ConversationSubsystem,
     MeetingNotifier,
     JobSubsystem,
     Input RequestId,
     FederationSubsystem,
     TeamCollaboratorsSubsystem,
     Input AllTeamFeatures,
     FeaturesConfigSubsystem,
     TeamSubsystem,
     SparAPIAccess,
     NotificationSubsystem,
     ExternalAccess,
     BrigAPIAccess,
     GundeckAPIAccess,
     Rpc,
     FederationAPIAccess FederatorClient,
     BackendNotificationQueueAccess,
     FireAndForget,
     TeamCollaboratorsStore,
     MeetingsStore,
     UserClientIndexStore,
     CodeStore,
     ProposalStore,
     RateLimit,
     HashPassword,
     Random,
     CustomBackendStore,
     TeamStore,
     TeamJournal,
     LegalHoldStore,
     Input LegalHoldEnv,
     UserGroupStore,
     ServiceStore,
     TeamNotificationStore,
     ConversationStore,
     MLSCommitLockStore,
     TeamFeatureStore,
     TeamMemberStore InternalPaging,
     TeamMemberStore CassandraPaging,
     ListItems LegacyPaging TeamId,
     ListItems InternalPaging TeamId,
     Input ExposeInvitationURLsAllowlist,
     Input FeatureFlags,
     Input FanoutLimit,
     Input (FeatureDefaults LegalholdConfig),
     Input (Local ()),
     Input IntraListing,
     Input (Maybe GuestLinkTTLSeconds),
     Input (Maybe (MLSKeysByPurpose MLSPrivateKeys)),
     Input (Maybe GroupInfoCheckEnabled),
     Input Opts,
     Input (Either HttpsUrl (Map Domain HttpsUrl)),
     Now,
     BoundedQueue DeleteItem,
     Error MeetingError,
     Error DynError,
     Error RateLimitExceeded,
     Error ConversationSubsystemError,
     ErrorS OperationDenied,
     ErrorS 'AccessDenied,
     ErrorS 'TeamMemberNotFound,
     ErrorS 'HistoryNotSupported,
     ErrorS 'NotATeamMember,
     ErrorS 'ConvAccessDenied,
     ErrorS 'MLSNotEnabled,
     ErrorS 'MLSNonEmptyMemberList,
     ErrorS 'MissingLegalholdConsent,
     ErrorS 'NonBindingTeam,
     ErrorS 'NoBindingTeamMembers,
     ErrorS 'TeamNotFound,
     ErrorS 'InvalidOperation,
     ErrorS 'ConvNotFound,
     ErrorS 'ChannelsNotEnabled,
     ErrorS 'NotAnMlsConversation,
     ErrorS 'NotATeamMember,
     ErrorS 'MeetingNotFound,
     ErrorS 'CodeStoreNotFound,
     ErrorS 'InvalidOperation,
     Error RpcException,
     Input ClientState,
     Input HasqlPoolExt.Pool,
     Input Env,
     Input ConversationSubsystemConfig,
     Error MigrationLockError,
     Error TeamFeatureStoreError,
     Error MigrationError,
     Error InvalidInput,
     Error ParseException,
     Error InternalError,
     Error FederationError,
     Error UnreachableBackends,
     Error NonFederatingBackends,
     Error TeamCollaboratorsError,
     Error Hasql.UsageError,
     Error HttpError,
     Race,
     Async,
     Delay,
     Fail,
     TinyLog,
     Embed IO,
     Error JSONResponse,
     Resource,
     Concurrency 'Unsafe,
     Final IO
   ]

-- | Resolved meeting system config, with defaults 48h validity / 24h
-- past-edit period.
meetingSystemConfig :: Opts -> Meeting.MeetingSystemConfig
meetingSystemConfig o =
  Meeting.MeetingSystemConfig
    { legacyTimeZone = fromMaybe defaultLegacyTimeZone (m >>= view legacyTimeZone >>= parseTimeZone),
      validityPeriod = realToFrac $ maybe (secondsToDiffTime (48 * 3600)) (.duration) (m >>= view validityPeriod),
      pastEditPeriod = realToFrac $ maybe (secondsToDiffTime (24 * 3600)) (.duration) (m >>= view pastEditPeriod)
    }
  where
    m = o ^. settings . meetings

-- Define some invariants for the options used
validateOptions :: Opts -> IO (Either HttpsUrl (Map Domain HttpsUrl))
validateOptions o = do
  let settings' = view settings o
      optFanoutLimit = fromIntegral . fromRange $ currentFanoutLimit settings'._maxTeamSize settings'._maxFanoutSize
  when (settings'._maxConvSize > fromIntegral optFanoutLimit) $
    error "setMaxConvSize cannot be > setTruncationLimit"
  when (settings' ^. maxTeamSize < optFanoutLimit) $
    error "setMaxTeamSize cannot be < setTruncationLimit"
  let meetingsCfg = meetingSystemConfig o
  when (meetingsCfg.pastEditPeriod < 0 || meetingsCfg.pastEditPeriod > meetingsCfg.validityPeriod) $
    error "settings.meetings.pastEditPeriod must be non-negative and cannot be greater than settings.meetings.validityPeriod"
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
  conversationCodeURISettings o

createEnv :: Opts -> Logger -> IO Env
createEnv o l = do
  cass <- initCassandra o l
  mgr <- initHttpManager o
  h2mgr <- initHttp2Manager
  codeURIcfg <- validateOptions o
  postgres <- initPostgresPool o._postgresqlPool o._postgresql o._postgresqlPassword
  let disableTlsV1 = True
  Env (RequestId defRequestId) o l mgr h2mgr (o ^. O.federator) (o ^. O.brig) cass postgres
    <$> Q.new 16000
    <*> initExtEnv disableTlsV1
    <*> maybe (pure Nothing) (\jo -> fmap Just (Aws.mkEnv l mgr (jo ^. O.endpoint) (jo ^. O.queueName))) (o ^. journal)
    <*> traverse loadAllMLSKeys (o ^. settings . mlsPrivateKeyPaths)
    <*> traverse (mkRabbitMqChannelMVar l (Just "galley")) (o ^. rabbitmq)
    <*> pure codeURIcfg
    <*> newRateLimitEnv (o ^. settings . passwordHashingRateLimit)
    <*> initMeetingEmailEnv o l mgr

-- | Build the meeting-email environment when @settings.meetings.email@ is
-- configured; 'Nothing' (the default) disables meeting emails entirely.
initMeetingEmailEnv :: Opts -> Logger -> Manager -> IO (Maybe MeetingEmailEnv)
initMeetingEmailEnv o lgr mgr = case o ^. settings . meetings >>= (^. O.email) of
  Nothing -> pure Nothing
  Just cfg -> do
    templates <- loadMeetingTemplates (O._templateDir cfg) def (O._from cfg)
    let meetingBranding = fromMaybe Map.empty (O._branding cfg)
    (mSmtp, mAwsEnv) <- case O._transport cfg of
      EmailOpt.EmailSMTP s -> do
        let Cassandra.Options.Endpoint {host = h, port = p} = s.smtpEndpoint
            p' = Just . fromInteger . toInteger $ p
        smtpCredentials <- case EmailOpt.smtpCredentials s of
          Just (EmailOpt.EmailSMTPCredentials u pw) ->
            Just . (SMTP.Username u,) . SMTP.Password <$> initCredentials pw
          _ -> pure Nothing
        smtp <- SMTP.initSMTP lgr h p' smtpCredentials (EmailOpt.smtpConnType s)
        pure (Just smtp, Nothing)
      EmailOpt.EmailAWS ao -> do
        -- Mirror brig's SES endpoint construction (Brig.AWS.mkAwsEnv).
        let svc = Amazonka.setEndpoint (ao.sesEndpoint ^. awsSecure) (ao.sesEndpoint ^. awsHost) (ao.sesEndpoint ^. awsPort) SES.defaultService
        baseEnv <- Amazonka.newEnv Amazonka.discover <&> Amazonka.configureService svc
        pure
          ( Nothing,
            Just $
              baseEnv
                { Amazonka.logger = awsLogger (Log.clone (Just "aws.galley") lgr),
                  Amazonka.manager = mgr
                }
          )
    pure (Just MeetingEmailEnv {meTemplates = templates, meBranding = meetingBranding, meSmtp = mSmtp, meAwsEnv = mAwsEnv})
  where
    awsLogger g l = Log.log g (mapLevel l) . Log.msg . toLazyByteString
    mapLevel Amazonka.Info = Log.Info
    mapLevel Amazonka.Debug = Log.Debug
    mapLevel Amazonka.Trace = Log.Trace
    mapLevel Amazonka.Error = Log.Error

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

logAndMapError :: forall e1 e2 r a. (Member TinyLog r, Member (Error e2) r) => (e1 -> e2) -> (e1 -> Text) -> Text -> Sem (Error e1 : r) a -> Sem r a
logAndMapError fErr fLog logMsg action =
  mapError fErr $ logErrors @_ @e1 fLog logMsg action

evalGalley :: Env -> Sem GalleyEffects a -> ExceptT JSONResponse IO a
evalGalley e =
  let convStoreInterpreter =
        interpretConversationStoreByMigration (e ^. options . postgresMigration).conversation (e ^. cstate)
      convCodesStoreInterpreter =
        case (e ^. options . postgresMigration).conversationCodes of
          CassandraStorage -> interpretCodeStoreToCassandra
          MigrationToPostgresql -> interpretCodeStoreToCassandraAndPostgres
          PostgresqlStorage -> interpretCodeStoreToPostgres
      teamFeatureStoreInterpreter =
        case (e ^. options . postgresMigration).teamFeatures of
          CassandraStorage -> interpretTeamFeatureStoreToCassandra
          MigrationToPostgresql -> interpretTeamFeatureStoreToCassandraAndPostgres
          PostgresqlStorage -> interpretTeamFeatureStoreToPostgres
      localUnit = toLocalUnsafe (e ^. options . settings . federationDomain) ()
      teamSubsystemConfig =
        TeamSubsystemConfig
          { concurrentDeletionEvents = fromMaybe defConcurrentDeletionEvents e._options._settings._concurrentDeletionEvents
          }
      backendNotificationQueueAccessEnv =
        case e._rabbitmqChannel of
          Nothing -> Nothing
          Just chanMVar ->
            Just
              BackendNotificationQueueAccess.Env
                { BackendNotificationQueueAccess.channelMVar = chanMVar,
                  BackendNotificationQueueAccess.logger = e ^. applog,
                  BackendNotificationQueueAccess.local = localUnit,
                  BackendNotificationQueueAccess.requestId = e ^. reqId
                }
      federationAPIAccessConfig =
        FederationAPIAccessConfig
          { ownDomain = e._options._settings._federationDomain,
            federatorEndpoint = e._options._federator,
            http2Manager = e._http2Manager,
            requestId = e._reqId,
            localVersions =
              Federation.enabledFederationVersions e._options._settings._disabledFederationAPIVersions
          }
      conversationSubsystemConfig =
        ConversationSubsystemConfig
          { mlsKeys = e._mlsKeys,
            federationProtocols = e._options._settings._federationProtocols,
            legalholdDefaults = lh,
            maxConvSize = e._options._settings._maxConvSize,
            listClientsUsingBrig = e._options._settings._intraListing
          }
   in ExceptT
        . runFinal @IO
        . unsafelyPerformConcurrency
        . resourceToIOFinal
        . runError
        . embedToFinal @IO
        . interpretTinyLog e
        . failToEmbed @IO
        . runDelay
        . asyncToIOFinal
        . interpretRace
        . mapError httpErrorToJSONResponse
        . logAndMapError postgresUsageErrorToHttpError (Text.pack . show) "postgres usage error"
        . mapError teamCollaboratorsSubsystemErrorToHttpError
        . mapError toResponse
        . mapError toResponse
        . mapError toResponse
        . mapError toResponse
        . mapError toResponse
        . mapError toResponse
        . logAndMapError toResponse (Text.pack . show) "migration error"
        . mapError mapTeamFeatureStoreError
        . mapError toResponse
        . runInputConst conversationSubsystemConfig
        . runInputConst e
        . runInputConst (e ^. hasqlPool)
        . runInputConst (e ^. cstate)
        . mapError (toResponse . rpcExcepctionToWaiError) -- Error RpcException
        . mapError toResponse -- ErrorS 'InvalidOperation
        . mapError toResponse -- ErrorS 'MeetingNotFound
        . mapError toResponse -- ErrorS 'CodeStoreNotFound
        . mapError toResponse -- ErrorS 'NotATeamMember
        . mapError toResponse -- ErrorS 'NotAnMlsConversation
        . mapError toResponse -- ErrorS 'ChannelsNotEnabled
        . mapError toResponse -- ErrorS 'ConvNotFound
        . mapError toResponse -- ErrorS 'InvalidOperation
        . mapError toResponse -- ErrorS 'TeamNotFound
        . mapError toResponse -- ErrorS 'NoBindingTeamMembers
        . mapError toResponse -- ErrorS 'NonBindingTeam
        . mapError toResponse -- ErrorS 'MissingLegalholdConsent
        . mapError toResponse -- ErrorS 'MLSNonEmptyMemberList
        . mapError toResponse -- ErrorS 'MLSNotEnabled
        . mapError toResponse -- ErrorS 'ConvAccessDenied
        . mapError toResponse -- ErrorS 'NotATeamMember
        . mapError toResponse -- ErrorS 'HistoryNotSupported
        . mapError toResponse -- ErrorS 'TeamMemberNotFound
        . mapError toResponse -- ErrorS 'AccessDenied
        . mapError toResponse -- ErrorS OperationDenied
        . mapError toResponse -- Error ConversationSubsystemError,
        . mapError rateLimitExceededToHttpError
        . mapError toResponse -- DynError
        . mapError toResponse -- Error MeetingError
        . interpretBoundedQueue (e ^. deleteQueue)
        . nowToIO
        . runInputConst (e ^. convCodeURI)
        . runInputConst (e ^. options)
        . runInputConst (GroupInfoCheckEnabled <$> e._options._settings._checkGroupInfo)
        . runInputConst e._mlsKeys
        . runInputConst e._options._settings._guestLinkTTLSeconds
        . runInputConst (IntraListing e._options._settings._intraListing)
        . runInputConst localUnit
        . interpretTeamFeatureSpecialContext e
        . runInputConst (currentFanoutLimitOpts (e ^. options))
        . runInputSem (inputs @Opts $ view (O.settings . O.featureFlags))
        . runInputSem (inputs @Opts $ ExposeInvitationURLsAllowlist . fromMaybe [] . view (O.settings . O.exposeInvitationURLsTeamAllowlist))
        . interpretInternalTeamListToCassandra
        . interpretTeamListToCassandra
        . interpretTeamMemberStoreToCassandraWithPaging lh
        . interpretTeamMemberStoreToCassandra lh
        . teamFeatureStoreInterpreter
        . interpretMLSCommitLockStoreToCassandra (e ^. cstate)
        . convStoreInterpreter
        . interpretTeamNotificationStoreToCassandra
        . interpretServiceStoreToCassandra (e ^. cstate)
        . interpretUserGroupStoreToPostgres
        . runInputConst legalHoldEnv
        . interpretLegalHoldStoreToCassandra lh
        . interpretTeamJournal (e ^. aEnv)
        . interpretTeamStoreToCassandra
        . interpretCustomBackendStoreToCassandra
        . randomToIO
        . runHashPassword e._options._settings._passwordHashingOptions
        . interpretRateLimit e._passwordHashingRateLimitEnv
        . interpretProposalStoreToCassandra
        . convCodesStoreInterpreter
        . interpretUserClientIndexStoreToCassandra (e ^. cstate)
        . interpretMeetingsStoreToPostgres
        . interpretTeamCollaboratorsStoreToPostgres
        . interpretFireAndForget
        . BackendNotificationQueueAccess.interpretBackendNotificationQueueAccess backendNotificationQueueAccessEnv
        . interpretFederationAPIAccess federationAPIAccessConfig
        . runRpcWithHttp (e ^. manager) (e ^. reqId)
        . runGundeckAPIAccess (e ^. options . gundeck)
        . interpretBrigAccess (e ^. brig)
        . interpretExternalAccess (e ^. extEnv)
        . runNotificationSubsystemGundeck (notificationSubsystemConfig e)
        . interpretSparAPIAccessToRpc (e ^. options . spar)
        . interpretTeamSubsystem teamSubsystemConfig
        . runFeaturesConfigSubsystem
        . runInputSem getAllTeamFeaturesForServer
        . interpretTeamCollaboratorsSubsystem
        . runFederationSubsystem conversationSubsystemConfig.federationProtocols
        . runInputConst (e ^. reqId)
        . interpretJobSubsystem
          JobSubsystemConfig
            { jobSubsystemSchemaName = ArbiterCore.defaultSchemaName
            }
        . interpretMeetingNotifier
        . interpretConversationSubsystem
        . bestEffortEmailSending e
        . meetingEmailsInterpreter e
        . Meeting.interpretMeetingsSubsystem (meetingSystemConfig (e ^. options))
  where
    lh = view (options . settings . featureFlags . to npProject) e
    legalHoldEnv =
      let makeReq fpr url rb = runApp e (LHInternal.makeVerifiedRequest fpr url rb)
          makeReqFresh fpr url rb = runApp e (LHInternal.makeVerifiedRequestFreshManager fpr url rb)
       in LegalHoldEnv {makeVerifiedRequest = makeReq, makeVerifiedRequestFreshManager = makeReqFresh}

interpretTeamFeatureSpecialContext :: Env -> Sem (Input (FeatureDefaults LegalholdConfig) ': r) a -> Sem r a
interpretTeamFeatureSpecialContext e =
  runInputConst
    (e ^. options . settings . featureFlags . to npProject)

mapTeamFeatureStoreError :: TeamFeatureStoreError -> InternalError
mapTeamFeatureStoreError (TeamFeatureStoreErrorInternalError msg) = InternalErrorWithDescription msg

-- | Interpret 'EmailSubsystem' for meeting emails only. When
-- @settings.meetings.email@ is unset, no emails are sent (the config
-- comment's contract). Otherwise the three meeting operations render and
-- send via the configured transport, with delivery failures logged and
-- swallowed so they never surface to the meeting creator.
meetingEmailsInterpreter ::
  (Member TinyLog r) =>
  Env ->
  InterpreterFor EmailSubsystem (EmailSending ': r)
meetingEmailsInterpreter e = case e ^. meetingEmail of
  Nothing -> discardEmailSubsystem
  Just me -> meetingEmailSubsystemInterpreter me.meTemplates me.meBranding

-- | EmailSending transport whose delivery failures are logged and swallowed,
-- so a failed email never fails the meeting operation that triggered it
-- (WPB-25932).
bestEffortEmailSending ::
  forall r.
  ( Member TinyLog r,
    Member P.Async r,
    Member (Embed IO) r,
    Member (Final IO) r
  ) =>
  Env ->
  InterpreterFor EmailSending r
bestEffortEmailSending e = case e ^. meetingEmail of
  -- No meeting-email configuration: nothing can reach the transport.
  Nothing -> interpret \case
    EmailSending.SendMail _ -> pure ()
  Just me -> interpret \case
    -- Fire-and-forget (the 'pushAsyncImpl' idiom): delivery is best-effort, a
    -- failed or slow send must never delay or fail the meeting operation.
    -- SES failures surface as typed errors and are logged; SMTP failures are
    -- plain IO exceptions in the forked thread and die with it.
    EmailSending.SendMail mail -> void . P.async $ do
      res <-
        errorToIOFinal @EmailSendingAWSError
          (fromExceptionSem @EmailSendingAWSError (runTransport (sendMail mail)))
      case res of
        Left err ->
          P.err $
            Log.msg (Log.val "Failed to send meeting email")
              . Log.field "error" (displayException err)
        Right _ -> pure ()
    where
      runTransport ::
        Sem (EmailSending ': Error EmailSendingAWSError ': r) a ->
        Sem (Error EmailSendingAWSError ': r) a
      runTransport = case me.meSmtp of
        Just smtp -> SMTP.emailViaSMTPInterpreter (e ^. applog) smtp
        Nothing -> case me.meAwsEnv of
          Just awsEnv -> emailViaSESInterpreter awsEnv
          -- Cannot happen: 'initMeetingEmailEnv' always initializes the
          -- transport matching the configured one.
          Nothing -> error "meeting emails configured without a transport"
