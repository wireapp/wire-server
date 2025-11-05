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
import Data.Qualified
import Imports
import Polysemy
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
import Wire.API.User.Saml (TTLError)
import Wire.BrigAPIAccess (BrigAPIAccess)
import Wire.BrigAPIAccess.Rpc (interpretBrigAccess)
import Wire.ParseException (ParseException, parseExceptionToHttpError)
import Wire.Rpc (Rpc, runRpcWithHttp)
import Wire.ScimSubsystem
import Wire.ScimSubsystem.Interpreter
import Wire.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIO)
import Wire.Sem.Random (Random)
import Wire.Sem.Random.IO (randomToIO)

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
    . ttlErrorToSparError
    . idpDbErrorToSparError
    . galleyAccessToHttp (sparCtxHttpManager ctx) (sparCtxHttpGalley ctx)
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
    . verdictFormatStoreToCassandra
    . aReqIDStoreToCassandra
    . assIDStoreToCassandra
    . sparRouteToServant (saml $ sparCtxOpts ctx)
    . saml2ToSaml2WebSso
    . interpretBrigAccess ctx.sparCtxOpts.brig
    . interpretScimSubsystem

iParseException :: (Member (Error SparError) r) => Sem (Error ParseException : r) a -> Sem r a
iParseException = mapError (SAML.CustomError . SparSomeHttpError . parseExceptionToHttpError)

runSparToHandler :: Env -> Sem CanonicalEffs a -> Handler a
runSparToHandler ctx spar = do
  liftIO (runSparToIO ctx spar) >>= \case
    Right val -> pure val
    Left err -> sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
