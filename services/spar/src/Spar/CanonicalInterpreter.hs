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
import Control.Monad.Except
import Data.Qualified
import qualified Data.Text.Lazy as LText
import Imports
import Polysemy
import qualified Polysemy.Async as P
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.TinyLog hiding (err)
import qualified Polysemy.TinyLog as P
import qualified SAML2.WebSSO.Error as SAML
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
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc (interpretGalleyAPIAccessToRpc)
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter
import Wire.ParseException (ParseException)
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
import Wire.UserGroupSubsystem
import Wire.UserGroupSubsystem.Interpreter
import Wire.UserSubsystem

type CanonicalEffs =
  '[ SAML2,
     SamlProtocolSettings,
     AssIDStore,
     AReqIDStore,
     VerdictFormatStore,
     ScimSubsystem,
     UserGroupSubsystem,
     UserSubsystem,
     Error UserGroupSubsystemError,
     Store.UserGroupStore,
     NotificationSubsystem,
     GundeckAPIAccess,
     P.Async,
     Delay,
     TeamSubsystem,
     GalleyAPIAccess,
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
     Error IdpDbError,
     Error TTLError,
     Error SparError,
     Reporter,
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
runSparToIO ctx action =
  runFinal
    . embedToFinal @IO
    . nowToIO
    . randomToIO
    . runInputConst (sparCtxLogger ctx)
    . runInputConst (sparCtxOpts ctx)
    . loggerToTinyLog (sparCtxLogger ctx)
    . stringLoggerToTinyLog
    . reporterToTinyLogWai
    . runError @SparError
    . ttlErrorToSparError
    . idpDbErrorToSparError
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
    . Polysemy.Error.mapError (SAML.CustomError . SparInternalError . ("galley rpc: " <>) . LText.pack . show)
    . iGalleyAPIAccess ctx
    . intepreterTeamSubsystemToGalleyAPI
    . runDelay
    . P.asyncToIOFinal
    . iGundeckAPIAccess ctx
    . iNotificationSubsystem ctx
    . iUserGroupStore ctx
    . iUserGroupSubsystemError
    . iUserSubsystem ctx
    . interpretUserGroupSubsystem
    . interpretScimSubsystem
    . verdictFormatStoreToCassandra
    . aReqIDStoreToCassandra
    . assIDStoreToCassandra
    . sparRouteToServant (saml $ sparCtxOpts ctx)
    $ saml2ToSaml2WebSso action

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

iUserGroupStore :: Env -> InterpreterFor UserGroupStore r
iUserGroupStore _env = undefined

iUserGroupSubsystemError :: (Member (Error SparError) r) => InterpreterFor (Error UserGroupSubsystemError) r
iUserGroupSubsystemError = Polysemy.Error.mapError (SAML.CustomError . SparInternalError . ("galley rpc: " <>) . LText.pack . show)

iUserSubsystem :: Env -> InterpreterFor UserSubsystem r
iUserSubsystem =
  {-
    ( Member UserStore r,
      Member AppStore r,
      Member UserKeyStore r,
      Member GalleyAPIAccess r,
      Member BlockListStore r,
      Member DRS.DomainRegistrationStore r,
      Member (Concurrency 'Unsafe) r,
      Member (Error FederationError) r,
      Member (Error UserSubsystemError) r,
      Member (FederationAPIAccess fedM) r,
      Member DeleteQueue r,
      Member Events r,
      Member Now r,
      RunClient (fedM 'Brig),
      FederationMonad fedM,
      Typeable fedM,
      Member IndexedUserStore r,
      Member FederationConfigStore r,
      Member Metrics r,
      Member InvitationStore r,
      Member TinyLog r,
      Member (Input UserSubsystemConfig) r,
      Member TeamSubsystem r,
      Member UserGroupStore r
    ) =>
    Env ->
    InterpreterFor Store.UserGroupStore r
  iUserGroupStore = runUserSubsystem
  -}
  undefined

runSparToHandler :: Env -> Sem CanonicalEffs a -> Handler a
runSparToHandler ctx spar = do
  liftIO (runSparToIO ctx spar) >>= \case
    Right val -> pure val
    Left err -> sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
