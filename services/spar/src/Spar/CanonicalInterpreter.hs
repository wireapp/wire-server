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
import Control.Monad.Except hiding (mapError)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Polysemy.Internal.Kind
import Polysemy.TinyLog hiding (err)
import Servant
import Spar.App hiding (sparToServerErrorWithLogging)
import Spar.Error
import Spar.Options
import Spar.Orphans ()
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.AReqIDStore.Cassandra (aReqIDStoreToCassandra)
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.AssIDStore.Cassandra (assIDStoreToCassandra)
import Spar.Sem.SAML2 (SAML2)
import Spar.Sem.SAML2.Library (saml2ToSaml2WebSso)
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import Spar.Sem.SAMLUserStore.Cassandra (samlUserStoreToCassandra)
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import Spar.Sem.ScimExternalIdStore.Cassandra (scimExternalIdStoreToCassandra)
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import Spar.Sem.ScimTokenStore.Cassandra (scimTokenStoreToCassandra)
import Spar.Sem.Utils
import Spar.Sem.VerdictFormatStore (VerdictFormatStore)
import Spar.Sem.VerdictFormatStore.Cassandra (verdictFormatStoreToCassandra)
import qualified System.Logger as TinyLog
import Wire.API.Routes.Version (expandVersionExp)
import Wire.API.User.Saml (TTLError)
import Wire.BrigAPIAccess
import Wire.BrigAPIAccess.Rpc
import Wire.ClientSubsystem.Error (ClientError, clientErrorToHttpError)
import Wire.DefaultSsoStore (DefaultSsoCode)
import Wire.DefaultSsoStore.Cassandra (defaultSsoCodeToCassandra)
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc
import Wire.IdPConfigStore (IdPConfigStore)
import Wire.IdPConfigStore.Cassandra (idPToCassandra)
import Wire.IdPRawMetadataStore (IdPRawMetadataStore)
import Wire.IdPRawMetadataStore.Cassandra (idpRawMetadataStoreToCassandra)
import Wire.IdPSubsystem (IdPSubsystem)
import Wire.IdPSubsystem.Interpreter (IdPSubsystemError, interpretIdPSubsystem)
import Wire.ParseException (ParseException, parseExceptionToHttpError)
import Wire.Reporter (Reporter)
import Wire.Reporter.Wai (reporterToTinyLogWai)
import Wire.Rpc (Rpc, runRpcWithHttp)
import Wire.RpcException
import Wire.SamlProtocolSettings (SamlProtocolSettings)
import Wire.SamlProtocolSettings.Servant (sparRouteToServant)
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.ScimUserTimesStore (ScimUserTimesStore)
import Wire.ScimUserTimesStore.Cassandra (scimUserTimesStoreToCassandra)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)

type CanonicalEffs =
  '[IdPSubsystem, ScimSubsystem]
    `Append` LowerLevelCanonicalEffs

type LowerLevelCanonicalEffs =
  '[ GalleyAPIAccess,
     BrigAPIAccess,
     SAML2,
     SamlProtocolSettings,
     AssIDStore,
     AReqIDStore,
     VerdictFormatStore,
     Error ParseException,
     Error ClientError,
     Rpc,
     Input ScimSubsystemConfig,
     Error IdPSubsystemError,
     Error ScimSubsystemError,
     ScimExternalIdStore,
     ScimUserTimesStore,
     ScimTokenStore,
     DefaultSsoCode,
     IdPConfigStore,
     IdPRawMetadataStore,
     SAMLUserStore,
     Embed Cas.Client,
     Error IdpDbError,
     Error TTLError,
     Error RpcException,
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
runSparToIO ctx =
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
    . rpcExceptionToSparError
    . ttlErrorToSparError
    . idpDbErrorToSparError
    . interpretClientToIO (sparCtxCas ctx)
    . samlUserStoreToCassandra
    . idpRawMetadataStoreToCassandra
    . idPToCassandra
    . defaultSsoCodeToCassandra
    . scimTokenStoreToCassandra
    . scimUserTimesStoreToCassandra
    . scimExternalIdStoreToCassandra
    . mapScimSubsystemErrors
    . mapIdPSubsystemErrors
    . runInputConst (ctx.sparCtxScimSubsystemConfig)
    . runRpcWithHttp ctx.sparCtxHttpManager ctx.sparCtxRequestId
    . iClientException
    . iParseException
    . verdictFormatStoreToCassandra
    . aReqIDStoreToCassandra
    . assIDStoreToCassandra
    . sparRouteToServant (saml $ sparCtxOpts ctx)
    . saml2ToSaml2WebSso
    . interpretBrigAccess ctx.sparCtxOpts.brig
    . interpretGalleyAPIAccessToRpc
      (foldMap expandVersionExp (disabledAPIVersions . sparCtxOpts $ ctx))
      (galley . sparCtxOpts $ ctx)
    . interpretScimSubsystem
    . interpretIdPSubsystem (enableIdPByEmailDiscovery . sparCtxOpts $ ctx)

iParseException :: (Member (Error SparError) r) => InterpreterFor (Error ParseException) r
iParseException = Polysemy.Error.mapError (httpErrorToSparError . parseExceptionToHttpError)

iClientException :: (Member (Error SparError) r) => InterpreterFor (Error ClientError) r
iClientException = Polysemy.Error.mapError (httpErrorToSparError . clientErrorToHttpError)

runSparToHandler :: Env -> Sem CanonicalEffs a -> Handler a
runSparToHandler ctx spar = do
  liftIO (runSparToIO ctx spar) >>= \case
    Right val -> pure val
    Left err -> sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
