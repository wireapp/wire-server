{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.CanonicalInterpreter
  ( CanonicalEffs,
    runSparToIO,
    runSparToHandler,
  )
where

import qualified Cassandra as Cas
import Control.Exception (ErrorCall (..))
import Control.Lens ((^.))
import Control.Monad.Except hiding (mapError)
import Data.Qualified
import Data.ZAuth.CryptoSign (CryptoSign, runCryptoSign)
import qualified Hasql.Pool as Hasql
import Imports
import Polysemy
import qualified Polysemy.Async as P
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.Internal.Kind
import Polysemy.TinyLog hiding (err)
import qualified SAML2.WebSSO as SAML
import Servant
import Spar.App hiding (sparToServerErrorWithLogging)
import Spar.Error
import Spar.Options
import Spar.Orphans ()
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.AReqIDStore.Cassandra (aReqIDStoreToCassandra)
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.AssIDStore.Cassandra (assIDStoreToCassandra)
import Spar.Sem.BrigAccess (BrigAccess)
import Spar.Sem.BrigAccess.Http (brigAccessToHttp)
import Spar.Sem.DefaultSsoCode (DefaultSsoCode)
import Spar.Sem.DefaultSsoCode.Cassandra (defaultSsoCodeToCassandra)
import Spar.Sem.GalleyAccess (GalleyAccess)
import Spar.Sem.GalleyAccess.Http (galleyAccessToHttp)
import Spar.Sem.IdPConfigStore (IdPConfigStore)
import Spar.Sem.IdPConfigStore.Cassandra (idPToCassandra)
import Spar.Sem.IdPRawMetadataStore (IdPRawMetadataStore)
import Spar.Sem.IdPRawMetadataStore.Cassandra (idpRawMetadataStoreToCassandra)
import Spar.Sem.Reporter (Reporter)
import Spar.Sem.Reporter.Wai (reporterToTinyLogWai)
import Spar.Sem.SAML2 (SAML2)
import Spar.Sem.SAML2.Library (saml2ToSaml2WebSso)
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import Spar.Sem.SAMLUserStore.Cassandra (samlUserStoreToCassandra)
import Spar.Sem.SamlProtocolSettings (SamlProtocolSettings)
import Spar.Sem.SamlProtocolSettings.Servant (sparRouteToServant)
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import Spar.Sem.ScimExternalIdStore.Cassandra (scimExternalIdStoreToCassandra)
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import Spar.Sem.ScimTokenStore.Cassandra (scimTokenStoreToCassandra)
import Spar.Sem.ScimUserTimesStore (ScimUserTimesStore)
import Spar.Sem.ScimUserTimesStore.Cassandra (scimUserTimesStoreToCassandra)
import Spar.Sem.Utils (idpDbErrorToSparError, interpretClientToIO, ttlErrorToSparError)
import Spar.Sem.VerdictFormatStore (VerdictFormatStore)
import Spar.Sem.VerdictFormatStore.Cassandra (verdictFormatStoreToCassandra)
import qualified System.Logger as TinyLog
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import Wire.API.User.Saml
import Wire.AWSSubsystem (AWSSubsystem)
import Wire.AWSSubsystem.AWS (runAWSSubsystem)
import qualified Wire.AWSSubsystem.AWS as AWSI
import Wire.AppStore
import Wire.AppStore.Postgres
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Config
import Wire.AuthenticationSubsystem.Error
import Wire.AuthenticationSubsystem.Interpreter
import Wire.BlockListStore
import Wire.BlockListStore.Cassandra (interpretBlockListStoreToCassandra)
import Wire.ConnectionStore (ConnectionStore)
import Wire.ConnectionStore.Cassandra (connectionStoreToCassandra)
import Wire.DeleteQueue
import Wire.DeleteQueue.Interpreter (runDeleteQueue)
import Wire.DomainRegistrationStore
import Wire.DomainRegistrationStore.Cassandra (interpretDomainRegistrationStoreToCassandra)
import Wire.EmailSending (EmailSending)
import Wire.EmailSending.Core (EmailSendingInterpreterConfig (EmailSendingInterpreterConfig), emailSendingInterpreter)
import Wire.EmailSubsystem
import Wire.EmailSubsystem.Interpreter (emailSubsystemInterpreter)
import Wire.Error
import Wire.Events
import Wire.Events.Interpreter (runEvents)
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter (interpretFederationAPIAccess)
import Wire.FederationConfigStore
import Wire.FederationConfigStore.Cassandra (interpretFederationDomainConfig)
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.GundeckAPIAccess
import Wire.HashPassword
import Wire.HashPassword.Interpreter (runHashPassword)
import Wire.IndexedUserStore
import Wire.IndexedUserStore.ElasticSearch (interpretIndexedUserStoreES)
import Wire.InvitationStore
import Wire.InvitationStore.Cassandra (interpretInvitationStoreToCassandra)
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter
import Wire.ParseException (ParseException, parseExceptionToHttpError)
import Wire.PasswordResetCodeStore
import Wire.PasswordResetCodeStore.Cassandra (passwordResetCodeStoreToCassandra)
import Wire.PasswordStore (PasswordStore)
import Wire.PasswordStore.Cassandra (interpretPasswordStore)
import Wire.RateLimit
import Wire.RateLimit.Interpreter (interpretRateLimit)
import Wire.Rpc (Rpc, runRpcWithHttp)
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO (unsafelyPerformConcurrency)
import Wire.Sem.Delay
import Wire.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog)
import Wire.Sem.Metrics
import Wire.Sem.Metrics.IO (runMetricsToIO)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)
import Wire.SessionStore
import Wire.SessionStore.Cassandra (interpretSessionStoreCassandra)
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI
import Wire.UserGroupStore
import qualified Wire.UserGroupStore as Store
import Wire.UserGroupStore.Postgres
import Wire.UserGroupSubsystem
import Wire.UserGroupSubsystem.Interpreter
import Wire.UserKeyStore
import Wire.UserKeyStore.Cassandra (interpretUserKeyStoreCassandra)
import Wire.UserStore
import Wire.UserStore.Cassandra
import Wire.UserSubsystem (UserSubsystem)
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.Interpreter

type CanonicalEffs =
  '[ScimSubsystem, UserGroupSubsystem, UserSubsystem, AuthenticationSubsystem]
    `Append` LowerLevelCanonicalEffs

type LowerLevelCanonicalEffs =
  '[ SAML2,
     SamlProtocolSettings,
     AssIDStore,
     AReqIDStore,
     VerdictFormatStore,
     Error UserGroupSubsystemError,
     Store.UserGroupStore,
     Events,
     AWSSubsystem,
     NotificationSubsystem,
     GundeckAPIAccess,
     P.Async,
     Delay,
     TeamSubsystem,
     GalleyAPIAccess
   ]
    `Append` AuthSubsystemLowerEffects
    `Append` UserSubsystemLowerEffects
    `Append` '[ Error ErrorCall,
                Error ParseException,
                Rpc,
                Input (Local ()),
                Input ScimSubsystemConfig,
                Error ScimSubsystemError,
                ScimExternalIdStore,
                ScimUserTimesStore,
                ScimTokenStore,
                DefaultSsoCode,
                IdPConfigStore,
                IdPRawMetadataStore,
                SAMLUserStore,
                Embed Cas.Client,
                BrigAccess,
                GalleyAccess,
                UserStore,
                Error RateLimitExceeded,
                Error IdpDbError,
                Error TTLError,
                Input Hasql.Pool,
                Error Hasql.UsageError,
                Error SparError,
                Reporter,
                EmailSending,
                Logger String,
                Logger (TinyLog.Msg -> TinyLog.Msg),
                Input Opts,
                Input TinyLog.Logger,
                Random,
                Now,
                Embed IO,
                Final IO
              ]

runSparToIO :: Env -> Sem CanonicalEffs a -> IO (Either SparError a)
runSparToIO ctx =
  runFinal
    . embedToFinal @IO
    . nowToIO
    . randomToIO
    . runInputConst (sparCtxLogger ctx)
    . runInputConst (sparCtxOpts ctx)
    . loggerToTinyLog (sparCtxLogger ctx)
    . stringLoggerToTinyLog
    . emailSendingInterpreter (EmailSendingInterpreterConfig ctx.sparCtxSmtp (ctx.sparCtxAws ^. AWSI.amazonkaEnv) ctx.sparCtxLogger)
    . reporterToTinyLogWai
    . runError @SparError
    . iHasqlUsageError
    . runInputConst ctx.sparCtxHasqlPool
    . ttlErrorToSparError
    . idpDbErrorToSparError
    . mapError (httpErrorToSparError . rateLimitExceededToHttpError)
    . interpretUserStoreCassandra ctx.sparCtxCas
    . galleyAccessToHttp (sparCtxHttpManager ctx) (sparCtxHttpGalley ctx)
    . brigAccessToHttp (sparCtxHttpManager ctx) (sparCtxHttpBrig ctx)
    . interpretClientToIO (sparCtxCas ctx)
    . samlUserStoreToCassandra
    . idpRawMetadataStoreToCassandra
    . idPToCassandra
    . defaultSsoCodeToCassandra
    . scimTokenStoreToCassandra
    . scimUserTimesStoreToCassandra
    . scimExternalIdStoreToCassandra
    . mapScimSubsystemErrors
    . runInputConst (ctx.sparCtxScimSubsystemConfig)
    . runInputConst (ctx.sparCtxLocalUnit)
    . runRpcWithHttp ctx.sparCtxHttpManager ctx.sparCtxRequestId
    . iParseException
    . iErrorCall
    . interpretUserSubsystemLowerEffects ctx
    . interpretAuthSubsystemLowerEffects ctx
    . iGalleyAPIAccess ctx
    . intepreterTeamSubsystemToGalleyAPI
    . runDelay
    . P.asyncToIOFinal
    . iGundeckAPIAccess ctx
    . iNotificationSubsystem ctx
    . runAWSSubsystem ctx.sparCtxAws
    . runEvents
    . iUserGroupStore
    . iUserGroupSubsystemError
    . verdictFormatStoreToCassandra
    . aReqIDStoreToCassandra
    . assIDStoreToCassandra
    . sparRouteToServant (saml $ sparCtxOpts ctx)
    . saml2ToSaml2WebSso
    . iUserAuthDoubleSubsystem
    . interpretUserGroupSubsystem
    . interpretScimSubsystem

iUserAuthDoubleSubsystem :: (Members LowerLevelCanonicalEffs r) => InterpretersFor '[UserSubsystem, AuthenticationSubsystem] r
iUserAuthDoubleSubsystem = authSubsystemInterpreter . userSubsystemInterpreter
  where
    userSubsystemInterpreter :: (Members LowerLevelCanonicalEffs r) => InterpreterFor UserSubsystem r
    userSubsystemInterpreter = runUserSubsystem authSubsystemInterpreter

    authSubsystemInterpreter :: (Members LowerLevelCanonicalEffs r) => InterpreterFor AuthenticationSubsystem r
    authSubsystemInterpreter = interpretAuthenticationSubsystem userSubsystemInterpreter

iGalleyAPIAccess ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member TinyLog r
  ) =>
  Env ->
  InterpreterFor GalleyAPIAccess r
iGalleyAPIAccess env = interpretGalleyAPIAccessToRpc env.disabledVersions env.sparCtxHttpGalleyEndpoint

iGundeckAPIAccess ::
  ( Member (Embed IO) r,
    Member Rpc r
  ) =>
  Env ->
  InterpreterFor GundeckAPIAccess r
iGundeckAPIAccess env = runGundeckAPIAccess (sparCtxHttpGundeckEndpoint env)

iNotificationSubsystem ::
  ( Member GundeckAPIAccess r,
    Member TinyLog r,
    Member Delay r,
    Member P.Async r,
    Member (Final IO) r
  ) =>
  Env ->
  InterpreterFor NotificationSubsystem r
iNotificationSubsystem env = runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig env.sparCtxRequestId)

iUserGroupStore ::
  ( Member (Input (Local ())) r,
    Member (Embed IO) r,
    Member (Input Hasql.Pool) r,
    Member (Error Hasql.UsageError) r
  ) =>
  InterpreterFor UserGroupStore r
iUserGroupStore = interpretUserGroupStoreToPostgres

iUserGroupSubsystemError :: (Member (Error SparError) r) => InterpreterFor (Error UserGroupSubsystemError) r
iUserGroupSubsystemError = Polysemy.Error.mapError (httpErrorToSparError . userGroupSubsystemErrorToHttpError)

iHasqlUsageError :: (Member (Error SparError) r) => InterpreterFor (Error Hasql.UsageError) r
iHasqlUsageError = Polysemy.Error.mapError (httpErrorToSparError . postgresUsageErrorToHttpError)

iParseException :: (Member (Error SparError) r) => InterpreterFor (Error ParseException) r
iParseException = Polysemy.Error.mapError (httpErrorToSparError . parseExceptionToHttpError)

iErrorCall :: (Member (Error SparError) r) => InterpreterFor (Error ErrorCall) r
iErrorCall = Polysemy.Error.mapError errorCallToSparError
  where
    errorCallToSparError :: ErrorCall -> SparError
    errorCallToSparError (ErrorCallWithLocation msg _) = SAML.CustomError (SparInternalError (fromString msg))

type UserSubsystemLowerEffects =
  '[ UserStore,
     AppStore,
     UserKeyStore,
     BlockListStore,
     ConnectionStore InternalPaging,
     DomainRegistrationStore,
     FederationAPIAccess FederatorClient,
     Concurrency 'Unsafe,
     Error FederationError,
     Error UserSubsystemError,
     DeleteQueue,
     IndexedUserStore,
     FederationConfigStore,
     Metrics,
     InvitationStore,
     Input UserSubsystemConfig
   ]

interpretUserSubsystemLowerEffects ::
  ( Member (Input Hasql.Pool) r,
    Member UserStore r,
    Member (Error Hasql.UsageError) r,
    Member (Error SparError) r,
    Member (Final IO) r,
    Member (Embed IO) r,
    Member (Embed Cas.Client) r,
    Member TinyLog r,
    Member (Error ErrorCall) r
  ) =>
  Env ->
  InterpretersFor UserSubsystemLowerEffects r
interpretUserSubsystemLowerEffects env =
  runInputConst env.sparCtxUserSubsystemConfig
    . interpretInvitationStoreToCassandra env.sparCtxCas
    . runMetricsToIO
    . interpretFederationDomainConfig env.sparCtxCas Nothing mempty
    . interpretIndexedUserStoreES env.sparCtxIndexedUserStoreConfig
    . runDeleteQueue env.sparCtxInternalEvents
    . mapError (httpErrorToSparError . userSubsystemErrorToHttpError)
    . mapError (httpErrorToSparError . StdError . federationErrorToWai)
    . unsafelyPerformConcurrency
    . interpretFederationAPIAccess env.sparCtxFederationAPIAccessConfig
    . interpretDomainRegistrationStoreToCassandra env.sparCtxCas
    . connectionStoreToCassandra
    . interpretBlockListStoreToCassandra env.sparCtxCas
    . interpretUserKeyStoreCassandra env.sparCtxCas
    . interpretAppStoreToPostgres
    . interpretUserStoreCassandra env.sparCtxCas

type AuthSubsystemLowerEffects =
  '[ PasswordResetCodeStore,
     Error AuthenticationSubsystemError,
     HashPassword,
     SessionStore,
     Input AuthenticationSubsystemConfig,
     PasswordStore,
     EmailSubsystem,
     RateLimit,
     CryptoSign,
     Random
   ]

interpretAuthSubsystemLowerEffects ::
  ( Member (Error SparError) r,
    Member (Embed Cas.Client) r,
    Member EmailSending r,
    Member (Error RateLimitExceeded) r,
    Member (Logger (TinyLog.Msg -> TinyLog.Msg)) r,
    Member (Embed IO) r
  ) =>
  Env ->
  InterpretersFor AuthSubsystemLowerEffects r
interpretAuthSubsystemLowerEffects env =
  randomToIO
    . runCryptoSign
    . interpretRateLimit env.sparCtxRateLimit
    . emailSubsystemInterpreter env.sparCtxUserTemplates env.sparCtxTeamTemplates env.sparCtxTemplateBranding
    . interpretPasswordStore env.sparCtxCas
    . runInputConst env.sparCtxAuthenticationSubsystemConfig
    . interpretSessionStoreCassandra env.sparCtxCas
    . runHashPassword env.sparCtxPasswordHashingOptions
    . mapError (httpErrorToSparError . authenticationSubsystemErrorToHttpError)
    . passwordResetCodeStoreToCassandra @Cas.Client

runSparToHandler :: Env -> Sem CanonicalEffs a -> Handler a
runSparToHandler ctx spar = do
  liftIO (runSparToIO ctx spar) >>= \case
    Right val -> pure val
    Left err -> sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
