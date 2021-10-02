{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.CanonicalInterpreter where

import qualified Cassandra as Cas
import Control.Monad.Except
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, runInputConst)
import Servant
import Spar.App hiding (sparToServerErrorWithLogging)
import Spar.Error
import Spar.Orphans ()
import Spar.Sem.AReqIDStore (AReqIDStore)
import Spar.Sem.AReqIDStore.Cassandra (aReqIDStoreToCassandra, ttlErrorToSparError)
import Spar.Sem.AssIDStore (AssIDStore)
import Spar.Sem.AssIDStore.Cassandra (assIDStoreToCassandra)
import Spar.Sem.BindCookieStore (BindCookieStore)
import Spar.Sem.BindCookieStore.Cassandra (bindCookieStoreToCassandra)
import Spar.Sem.BrigAccess (BrigAccess)
import Spar.Sem.BrigAccess.Http (brigAccessToHttp)
import Spar.Sem.DefaultSsoCode (DefaultSsoCode)
import Spar.Sem.DefaultSsoCode.Cassandra (defaultSsoCodeToCassandra)
import Spar.Sem.GalleyAccess (GalleyAccess)
import Spar.Sem.GalleyAccess.Http (galleyAccessToHttp)
import qualified Spar.Sem.IdP as IdPEffect
import Spar.Sem.IdP.Cassandra (idPToCassandra)
import Spar.Sem.Logger (Logger)
import Spar.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog)
import Spar.Sem.Now (Now)
import Spar.Sem.Now.IO (nowToIO)
import Spar.Sem.Random (Random)
import Spar.Sem.Random.IO (randomToIO)
import Spar.Sem.Reporter (Reporter)
import Spar.Sem.Reporter.Wai (reporterToWai)
import Spar.Sem.SAML2 (SAML2)
import Spar.Sem.SAML2.Library (saml2ToSaml2WebSso)
import Spar.Sem.SAMLUserStore (SAMLUserStore)
import Spar.Sem.SAMLUserStore.Cassandra (interpretClientToIO, samlUserStoreToCassandra)
import Spar.Sem.SamlProtocolSettings (SamlProtocolSettings)
import Spar.Sem.SamlProtocolSettings.Servant (sparRouteToServant)
import Spar.Sem.ScimExternalIdStore (ScimExternalIdStore)
import Spar.Sem.ScimExternalIdStore.Cassandra (scimExternalIdStoreToCassandra)
import Spar.Sem.ScimTokenStore (ScimTokenStore)
import Spar.Sem.ScimTokenStore.Cassandra (scimTokenStoreToCassandra)
import Spar.Sem.ScimUserTimesStore (ScimUserTimesStore)
import Spar.Sem.ScimUserTimesStore.Cassandra (scimUserTimesStoreToCassandra)
import qualified System.Logger as TinyLog
import Wire.API.User.Saml

type CanonicalEffs =
  '[ SAML2,
     SamlProtocolSettings,
     BindCookieStore,
     AssIDStore,
     AReqIDStore,
     ScimExternalIdStore,
     ScimUserTimesStore,
     ScimTokenStore,
     DefaultSsoCode,
     IdPEffect.IdP,
     SAMLUserStore,
     Embed (Cas.Client),
     BrigAccess,
     GalleyAccess,
     Error TTLError,
     Error SparError,
     Reporter,
     -- TODO(sandy): Make this a Logger Text instead
     Logger String,
     Logger (TinyLog.Msg -> TinyLog.Msg),
     Input Opts,
     Input TinyLog.Logger,
     Random,
     Now,
     Embed IO,
     Final IO
   ]

runSparToIO :: Env -> Spar CanonicalEffs a -> IO (Either SparError a)
runSparToIO ctx action =
  runFinal
    . embedToFinal @IO
    . nowToIO
    . randomToIO
    . runInputConst (sparCtxLogger ctx)
    . runInputConst (sparCtxOpts ctx)
    . loggerToTinyLog (sparCtxLogger ctx)
    . stringLoggerToTinyLog
    . reporterToWai
    . runError @SparError
    . ttlErrorToSparError
    . galleyAccessToHttp (sparCtxHttpManager ctx) (sparCtxHttpGalley ctx)
    . brigAccessToHttp (sparCtxHttpManager ctx) (sparCtxHttpBrig ctx)
    . interpretClientToIO (sparCtxCas ctx)
    . samlUserStoreToCassandra
    . idPToCassandra
    . defaultSsoCodeToCassandra
    . scimTokenStoreToCassandra
    . scimUserTimesStoreToCassandra
    . scimExternalIdStoreToCassandra
    . aReqIDStoreToCassandra
    . assIDStoreToCassandra
    . bindCookieStoreToCassandra
    . sparRouteToServant (saml $ sparCtxOpts ctx)
    $ saml2ToSaml2WebSso action

runSparToHandler :: Env -> Spar CanonicalEffs a -> Handler a
runSparToHandler ctx spar = do
  err <- liftIO $ runSparToIO ctx spar
  throwErrorAsHandlerException err
  where
    throwErrorAsHandlerException :: Either SparError a -> Handler a
    throwErrorAsHandlerException (Left err) =
      sparToServerErrorWithLogging (sparCtxLogger ctx) err >>= throwError
    throwErrorAsHandlerException (Right a) = pure a
