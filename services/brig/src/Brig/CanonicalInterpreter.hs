module Brig.CanonicalInterpreter where

import Brig.AWS (amazonkaEnv)
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
import Brig.User.Phone qualified as Brig
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
import Wire.API.Allowlists (AllowlistEmailDomains, AllowlistPhonePrefixes)
import Wire.API.Federation.Client qualified
import Wire.API.Federation.Error
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Interpreter
import Wire.DeleteQueue
import Wire.EmailSending
import Wire.EmailSending.SES
import Wire.EmailSending.SMTP
import Wire.EmailSmsSubsystem
import Wire.EmailSmsSubsystem.Interpreter
import Wire.EmailSmsSubsystem.Template (Localised, TemplateBranding, UserTemplates)
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
import Wire.PasswordStore (PasswordStore)
import Wire.PasswordStore.Cassandra (interpretPasswordStore)
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Delay
import Wire.Sem.Jwk
import Wire.Sem.Logger.TinyLog (loggerToTinyLogReqId)
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
     EmailSmsSubsystem,
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
     PasswordStore,
     SFT,
     ConnectionStore InternalPaging,
     Input UTCTime,
     Input (Local ()),
     Input (Maybe AllowlistEmailDomains),
     Input (Maybe AllowlistPhonePrefixes),
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
     EmailSending,
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
              . loggerToTinyLogReqId (e ^. App.requestId) (e ^. applog)
              . runError @SomeException
              . mapError @ErrorCall SomeException
              . mapError @ParseException SomeException
              . interpretClientToIO (e ^. casClient)
              . runRpcWithHttp (e ^. httpManager) (e ^. App.requestId)
              . emailSendingInterpreter e
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
              . runInputConst (e ^. settings . Opt.allowlistPhonePrefixes)
              . runInputConst (e ^. settings . Opt.allowlistEmailDomains)
              . runInputConst (toLocalUnsafe (e ^. settings . Opt.federationDomain) ())
              . runInputSem (embed getCurrentTime)
              . connectionStoreToCassandra
              . interpretSFT (e ^. httpManager)
              . interpretPasswordStore (e ^. casClient)
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
              . emailSmsSubsystemInterpreter e (e ^. usrTemplates) (e ^. templateBranding)
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

emailSendingInterpreter :: (Member (Embed IO) r) => Env -> InterpreterFor EmailSending r
emailSendingInterpreter e = do
  case (e ^. smtpEnv) of
    Just smtp -> emailViaSMTPInterpreter (e ^. applog) smtp
    Nothing -> emailViaSESInterpreter (e ^. awsEnv . amazonkaEnv)

-- FUTUREWORK: Env can be removed once phone users are removed, and then this interpreter should go to wire-subsystems
emailSmsSubsystemInterpreter :: (Member (Final IO) r, Member EmailSending r) => Env -> Localised UserTemplates -> TemplateBranding -> InterpreterFor EmailSmsSubsystem r
emailSmsSubsystemInterpreter e tpls branding = interpret \case
  SendPasswordResetMail email (key, code) mLocale -> sendPasswordResetMailImpl tpls branding email key code mLocale
  SendPasswordResetSms phone keyCodePair mLocale -> flip runReaderT e $ unAppT $ wrapHttp do
    Brig.sendPasswordResetSms phone keyCodePair mLocale
  SendVerificationMail email key code mLocale -> sendVerificationMailImpl tpls branding email key code mLocale
  SendTeamDeletionVerificationMail email code mLocale -> sendTeamDeletionVerificationMailImpl tpls branding email code mLocale
  SendCreateScimTokenVerificationMail email code mLocale -> sendCreateScimTokenVerificationMailImpl tpls branding email code mLocale
  SendLoginVerificationMail email code mLocale -> sendLoginVerificationMailImpl tpls branding email code mLocale
  SendActivationMail email name key code mLocale -> sendActivationMailImpl tpls branding email name key code mLocale
  SendActivationUpdateMail email name key code mLocale -> sendActivationUpdateMailImpl tpls branding email name key code mLocale
  SendTeamActivationMail email name key code mLocale teamName -> sendTeamActivationMailImpl tpls branding email name key code mLocale teamName
  SendNewClientEmail email name client locale -> sendNewClientEmailImpl tpls branding email name client locale
  SendDeletionEmail email name key code locale -> sendDeletionEmailImpl tpls branding email name key code locale
