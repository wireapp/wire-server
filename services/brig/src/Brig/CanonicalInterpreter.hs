module Brig.CanonicalInterpreter where

import Brig.AWS (amazonkaEnv)
import Brig.App as App
import Brig.DeleteQueue.Interpreter as DQ
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.ConnectionStore.Cassandra (connectionStoreToCassandra)
import Brig.Effects.FederationConfigStore (FederationConfigStore)
import Brig.Effects.FederationConfigStore.Cassandra (interpretFederationDomainConfig, remotesMapFromCfgFile)
import Brig.Effects.JwtTools
import Brig.Effects.PublicKeyBundle
import Brig.Effects.SFT (SFT, interpretSFT)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.IO.Intra (runEvents)
import Brig.Options (ImplicitNoFederationRestriction (federationDomainConfig), federationDomainConfigs, federationStrategy)
import Brig.Options qualified as Opt
import Cassandra qualified as Cas
import Control.Exception (ErrorCall)
import Control.Lens (to, (^.))
import Control.Monad.Catch (throwM)
import Data.Qualified (Local, toLocalUnsafe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Embed (runEmbedded)
import Polysemy.Error (Error, errorToIOFinal, mapError, runError)
import Polysemy.Fail
import Polysemy.Input (Input, runInputConst, runInputSem)
import Polysemy.TinyLog (TinyLog)
import Wire.API.Allowlists (AllowlistEmailDomains)
import Wire.API.Federation.Client qualified
import Wire.API.Federation.Error
import Wire.ActivationCodeStore (ActivationCodeStore)
import Wire.ActivationCodeStore.Cassandra (interpretActivationCodeStoreToCassandra)
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Interpreter
import Wire.BlockListStore
import Wire.BlockListStore.Cassandra
import Wire.DeleteQueue
import Wire.EmailSending
import Wire.EmailSending.SES
import Wire.EmailSending.SMTP
import Wire.EmailSubsystem
import Wire.EmailSubsystem.Interpreter
import Wire.Error
import Wire.Events
import Wire.FederationAPIAccess qualified
import Wire.FederationAPIAccess.Interpreter (FederationAPIAccessConfig (..), interpretFederationAPIAccess)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess.Rpc
import Wire.GundeckAPIAccess
import Wire.HashPassword
import Wire.InvitationCodeStore (InvitationCodeStore)
import Wire.InvitationCodeStore.Cassandra (interpretInvitationCodeStoreToCassandra)
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter (defaultNotificationSubsystemConfig, runNotificationSubsystemGundeck)
import Wire.ParseException
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PasswordResetCodeStore.Cassandra (interpretClientToIO, passwordResetCodeStoreToCassandra)
import Wire.PasswordStore (PasswordStore)
import Wire.PasswordStore.Cassandra (interpretPasswordStore)
import Wire.PropertyStore
import Wire.PropertyStore.Cassandra
import Wire.PropertySubsystem
import Wire.PropertySubsystem.Interpreter
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Delay
import Wire.Sem.Jwk
import Wire.Sem.Logger.TinyLog (loggerToTinyLogReqId)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.Sem.Random
import Wire.Sem.Random.IO
import Wire.SessionStore
import Wire.SessionStore.Cassandra (interpretSessionStoreCassandra)
import Wire.UserKeyStore
import Wire.UserKeyStore.Cassandra
import Wire.UserStore
import Wire.UserStore.Cassandra
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.Interpreter
import Wire.VerificationCodeStore
import Wire.VerificationCodeStore.Cassandra
import Wire.VerificationCodeSubsystem
import Wire.VerificationCodeSubsystem.Interpreter

type BrigCanonicalEffects =
  '[ AuthenticationSubsystem,
     UserSubsystem,
     EmailSubsystem,
     VerificationCodeSubsystem,
     PropertySubsystem,
     DeleteQueue,
     Wire.Events.Events,
     Error UserSubsystemError,
     Error AuthenticationSubsystemError,
     Error Wire.API.Federation.Error.FederationError,
     Error VerificationCodeSubsystemError,
     Error PropertySubsystemError,
     Error HttpError,
     Wire.FederationAPIAccess.FederationAPIAccess Wire.API.Federation.Client.FederatorClient,
     HashPassword,
     UserKeyStore,
     UserStore,
     SessionStore,
     PasswordStore,
     VerificationCodeStore,
     ActivationCodeStore,
     InvitationCodeStore,
     PropertyStore,
     SFT,
     ConnectionStore InternalPaging,
     Input VerificationCodeThrottleTTL,
     Input UTCTime,
     Input (Local ()),
     Input (Maybe AllowlistEmailDomains),
     NotificationSubsystem,
     GundeckAPIAccess,
     FederationConfigStore,
     Jwk,
     PublicKeyBundle,
     JwtTools,
     BlockListStore,
     UserPendingActivationStore InternalPaging,
     Now,
     Delay,
     Random,
     PasswordResetCodeStore,
     GalleyAPIAccess,
     EmailSending,
     Rpc,
     Embed Cas.Client,
     Error ParseException,
     Error ErrorCall,
     Error SomeException,
     TinyLog,
     Embed HttpClientIO,
     Fail,
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
      propertySubsystemConfig =
        PropertySubsystemConfig
          { maxKeyLength = fromMaybe Opt.defMaxKeyLen $ e ^. settings . Opt.propertyMaxKeyLen,
            maxValueLength = fromMaybe Opt.defMaxValueLen $ e ^. settings . Opt.propertyMaxValueLen,
            maxProperties = 16
          }
  ( either throwM pure
      <=< ( runFinal
              . unsafelyPerformConcurrency
              . asyncToIOFinal
              . interpretRace
              . embedToFinal
              . failToEmbed @IO -- if a fallible pattern fails, we throw a hard IO error
              . runEmbedded (runHttpClientIO e)
              . loggerToTinyLogReqId (e ^. App.requestId) (e ^. applog)
              . runError @SomeException
              . mapError @ErrorCall SomeException
              . mapError @ParseException SomeException
              . interpretClientToIO (e ^. casClient)
              . runRpcWithHttp (e ^. httpManager) (e ^. App.requestId)
              . emailSendingInterpreter e
              . interpretGalleyAPIAccessToRpc (e ^. disabledVersions) (e ^. galleyEndpoint)
              . passwordResetCodeStoreToCassandra @Cas.Client
              . randomToIO
              . runDelay
              . nowToIOAction (e ^. currentTime)
              . userPendingActivationStoreToCassandra
              . interpretBlockListStoreToCassandra @Cas.Client
              . interpretJwtTools
              . interpretPublicKeyBundle
              . interpretJwk
              . interpretFederationDomainConfig (e ^. settings . federationStrategy) (foldMap (remotesMapFromCfgFile . fmap (.federationDomainConfig)) (e ^. settings . federationDomainConfigs))
              . runGundeckAPIAccess (e ^. gundeckEndpoint)
              . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig (e ^. App.requestId))
              . runInputConst (e ^. settings . Opt.allowlistEmailDomains)
              . runInputConst (toLocalUnsafe (e ^. settings . Opt.federationDomain) ())
              . runInputSem (embed getCurrentTime)
              . runInputConst (e ^. settings . to Opt.set2FACodeGenerationDelaySecs . to fromIntegral)
              . connectionStoreToCassandra
              . interpretSFT (e ^. httpManager)
              . interpretPropertyStoreCassandra (e ^. casClient)
              . interpretInvitationCodeStoreToCassandra (e ^. casClient)
              . interpretActivationCodeStoreToCassandra (e ^. casClient)
              . interpretVerificationCodeStoreCassandra (e ^. casClient)
              . interpretPasswordStore (e ^. casClient)
              . interpretSessionStoreCassandra (e ^. casClient)
              . interpretUserStoreCassandra (e ^. casClient)
              . interpretUserKeyStoreCassandra (e ^. casClient)
              . runHashPassword
              . interpretFederationAPIAccess federationApiAccessConfig
              . rethrowHttpErrorIO
              . mapError propertySubsystemErrorToHttpError
              . mapError verificationCodeSubsystemErrorToHttpError
              . mapError (StdError . federationErrorToWai)
              . mapError authenticationSubsystemErrorToHttpError
              . mapError userSubsystemErrorToHttpError
              . runEvents
              . runDeleteQueue (e ^. internalEvents)
              . interpretPropertySubsystem propertySubsystemConfig
              . interpretVerificationCodeSubsystem
              . emailSubsystemInterpreter (e ^. userTemplates) (e ^. templateBranding)
              . runUserSubsystem userSubsystemConfig
              . interpretAuthenticationSubsystem
          )
    )
    $ runReaderT ma e

rethrowHttpErrorIO :: (Member (Final IO) r) => InterpreterFor (Error HttpError) r
rethrowHttpErrorIO act = do
  eithError <- errorToIOFinal act
  case eithError of
    Left err -> embedToFinal $ throwM $ err
    Right a -> pure a

emailSendingInterpreter :: (Member (Embed IO) r) => Env -> InterpreterFor EmailSending r
emailSendingInterpreter e = do
  case (e ^. smtpEnv) of
    Just smtp -> emailViaSMTPInterpreter (e ^. applog) smtp
    Nothing -> emailViaSESInterpreter (e ^. awsEnv . amazonkaEnv)
