{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Brig.CanonicalInterpreter where

import Brig.App as App
import Brig.DeleteQueue.Interpreter as DQ
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore.Cassandra (interpretBlacklistPhonePrefixStoreToCassandra)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.ConnectionStore.Cassandra (connectionStoreToCassandra)
import Brig.Effects.FederationConfigStore (FederationConfigStore)
import Brig.Effects.FederationConfigStore.Cassandra (interpretFederationDomainConfig, remotesMapFromCfgFile)
import Brig.Effects.JwtTools
import Brig.Effects.PublicKeyBundle
import Brig.Effects.SFT (SFT, interpretSFT)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.IO.Intra (runUserEvents)
import Brig.Options (ImplicitNoFederationRestriction (federationDomainConfig), federationDomainConfigs, federationStrategy)
import Brig.Options qualified as Opt
import Cassandra qualified as Cas
import Control.Exception (ErrorCall)
import Control.Lens (to, (^.))
import Control.Monad.Catch (throwM)
import Data.Qualified (Local, toLocalUnsafe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Imports
import Network.Wai.Utilities qualified as Wai
import Polysemy
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Embed (runEmbedded)
import Polysemy.Error (Error, errorToIOFinal, mapError, runError)
import Polysemy.Input (Input, runInputConst, runInputSem)
import Polysemy.TinyLog (TinyLog)
import Wire.API.Federation.Client qualified
import Wire.API.Federation.Error
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Interpreter
import Wire.DeleteQueue
import Wire.FederationAPIAccess qualified
import Wire.FederationAPIAccess.Interpreter (FederationAPIAccessConfig (..), interpretFederationAPIAccess)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess.Rpc
import Wire.GundeckAPIAccess
import Wire.HashPassword
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter (defaultNotificationSubsystemConfig, runNotificationSubsystemGundeck)
import Wire.ParseException
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PasswordResetCodeStore.Cassandra (interpretClientToIO, passwordResetCodeStoreToCassandra)
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Delay
import Wire.Sem.Jwk
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.SessionStore
import Wire.SessionStore.Cassandra (interpretSessionStoreCassandra)
import Wire.UserEvents
import Wire.UserKeyStore
import Wire.UserKeyStore.Cassandra
import Wire.UserStore
import Wire.UserStore.Cassandra
import Wire.UserSubsystem
import Wire.UserSubsystem.Interpreter

type BrigCanonicalEffects =
  '[ AuthenticationSubsystem,
     UserSubsystem,
     DeleteQueue,
     UserEvents,
     Error UserSubsystemError,
     Error AuthenticationSubsystemError,
     Error Wire.API.Federation.Error.FederationError,
     Error Wai.Error,
     Wire.FederationAPIAccess.FederationAPIAccess Wire.API.Federation.Client.FederatorClient,
     HashPassword,
     UserKeyStore,
     UserStore,
     SessionStore,
     SFT,
     ConnectionStore InternalPaging,
     Input UTCTime,
     Input (Local ()),
     NotificationSubsystem,
     GundeckAPIAccess,
     FederationConfigStore,
     Jwk,
     PublicKeyBundle,
     JwtTools,
     BlacklistPhonePrefixStore,
     BlacklistStore,
     UserPendingActivationStore InternalPaging,
     Now,
     Delay,
     PasswordResetCodeStore,
     GalleyAPIAccess,
     Rpc,
     Embed Cas.Client,
     Error ParseException,
     Error ErrorCall,
     Error SomeException,
     TinyLog,
     Embed HttpClientIO,
     Embed IO,
     Race,
     Async,
     Concurrency 'Unsafe,
     Final IO
   ]

runBrigToIO :: App.Env -> AppT BrigCanonicalEffects a -> IO a
runBrigToIO e (AppT ma) = do
  let userSubsystemConfig =
        UserSubsystemConfig
          { emailVisibilityConfig = e ^. settings . Opt.emailVisibility,
            defaultLocale = e ^. settings . to Opt.setDefaultUserLocale
          }
      federationApiAccessConfig =
        FederationAPIAccessConfig
          { ownDomain = e ^. settings . Opt.federationDomain,
            federatorEndpoint = e ^. federator,
            http2Manager = e ^. App.http2Manager,
            requestId = e ^. App.requestId
          }
  ( either throwM pure
      <=< ( runFinal
              . unsafelyPerformConcurrency
              . asyncToIOFinal
              . interpretRace
              . embedToFinal
              . runEmbedded (runHttpClientIO e)
              . loggerToTinyLog (e ^. applog)
              . runError @SomeException
              . mapError @ErrorCall SomeException
              . mapError @ParseException SomeException
              . interpretClientToIO (e ^. casClient)
              . runRpcWithHttp (e ^. httpManager) (e ^. App.requestId)
              . interpretGalleyAPIAccessToRpc (e ^. disabledVersions) (e ^. galleyEndpoint)
              . passwordResetCodeStoreToCassandra @Cas.Client
              . runDelay
              . nowToIOAction (e ^. currentTime)
              . userPendingActivationStoreToCassandra
              . interpretBlacklistStoreToCassandra @Cas.Client
              . interpretBlacklistPhonePrefixStoreToCassandra @Cas.Client
              . interpretJwtTools
              . interpretPublicKeyBundle
              . interpretJwk
              . interpretFederationDomainConfig (e ^. settings . federationStrategy) (foldMap (remotesMapFromCfgFile . fmap (.federationDomainConfig)) (e ^. settings . federationDomainConfigs))
              . runGundeckAPIAccess (e ^. gundeckEndpoint)
              . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig (e ^. App.requestId))
              . runInputConst (toLocalUnsafe (e ^. settings . Opt.federationDomain) ())
              . runInputSem (embed getCurrentTime)
              . connectionStoreToCassandra
              . interpretSFT (e ^. httpManager)
              . interpretSessionStoreCassandra (e ^. casClient)
              . interpretUserStoreCassandra (e ^. casClient)
              . interpretUserKeyStoreCassandra (e ^. casClient)
              . runHashPassword
              . interpretFederationAPIAccess federationApiAccessConfig
              . rethrowWaiErrorIO
              . mapError federationErrorToWai
              . mapError authenticationSubsystemErrorToWai
              . mapError userSubsystemErrorToWai
              . runUserEvents
              . runDeleteQueue (e ^. internalEvents)
              . runUserSubsystem userSubsystemConfig
              . interpretAuthenticationSubsystem
          )
    )
    $ runReaderT ma e

rethrowWaiErrorIO :: (Member (Final IO) r) => InterpreterFor (Error Wai.Error) r
rethrowWaiErrorIO act = do
  eithError <- errorToIOFinal act
  case eithError of
    Left err -> embedToFinal $ throwM $ err
    Right a -> pure a
