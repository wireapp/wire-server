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
import Wire.API.User.Saml
import Wire.AWSSubsystem (AWSSubsystem)
import Wire.AWSSubsystem.AWS (runAWSSubsystem)
import qualified Wire.AWSSubsystem.AWS as AWSI
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess.Rpc (interpretBrigAccess)
import Wire.EmailSending (EmailSending)
import Wire.EmailSending.Core (EmailSendingInterpreterConfig (EmailSendingInterpreterConfig), emailSendingInterpreter)
import Wire.Error
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter
import Wire.ParseException (ParseException, parseExceptionToHttpError)
import Wire.RateLimit
import Wire.Rpc (Rpc, runRpcWithHttp)
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.Sem.Delay
import Wire.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI
import Wire.UserGroupStore
import qualified Wire.UserGroupStore as Store
import Wire.UserGroupStore.Postgres
import Wire.UserGroupSubsystem.Interpreter
import Wire.UserStore
import Wire.UserStore.Cassandra

type CanonicalEffs =
  '[ScimSubsystem]
    `Append` LowerLevelCanonicalEffs

type LowerLevelCanonicalEffs =
  '[ BrigAPIAccess,
     SAML2,
     SamlProtocolSettings,
     AssIDStore,
     AReqIDStore,
     VerdictFormatStore,
     Error UserGroupSubsystemError,
     Store.UserGroupStore,
     AWSSubsystem,
     NotificationSubsystem,
     GundeckAPIAccess,
     P.Async,
     Delay,
     TeamSubsystem,
     GalleyAPIAccess,
     Error ErrorCall,
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
    . iGalleyAPIAccess ctx
    . intepreterTeamSubsystemToGalleyAPI
    . runDelay
    . P.asyncToIOFinal
    . iGundeckAPIAccess ctx
    . iNotificationSubsystem ctx
    . runAWSSubsystem ctx.sparCtxAws
    . iUserGroupStore
    . iUserGroupSubsystemError
    . verdictFormatStoreToCassandra
    . aReqIDStoreToCassandra
    . assIDStoreToCassandra
    . sparRouteToServant (saml $ sparCtxOpts ctx)
    . saml2ToSaml2WebSso
    . interpretBrigAccess ctx.sparCtxOpts.brig
    . interpretScimSubsystem

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

runSparToHandler :: Env -> Sem CanonicalEffs a -> Handler a
runSparToHandler ctx spar = do
  liftIO (runSparToIO ctx spar) >>= \case
    Right val -> pure val
    Left err -> sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
