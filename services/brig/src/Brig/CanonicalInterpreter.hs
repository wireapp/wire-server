module Brig.CanonicalInterpreter where

import Brig.AWS (amazonkaEnv)
import Brig.App as App
import Brig.DeleteQueue.Interpreter as DQ
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.ConnectionStore.Cassandra (connectionStoreToCassandra)
import Brig.Effects.JwtTools
import Brig.Effects.PublicKeyBundle
import Brig.Effects.SFT (SFT, interpretSFT)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.IO.Intra (runEvents)
import Brig.Options (federationDomainConfigs, federationStrategy)
import Brig.Options qualified as Opt
import Brig.Team.Template (TeamTemplates)
import Brig.User.Search.Index (IndexEnv (..))
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
import Polysemy.Input (Input, runInputConst, runInputSem)
import Polysemy.Internal.Kind
import Polysemy.TinyLog (TinyLog)
import Util.Options (PasswordHashingOptions)
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
import Wire.FederationConfigStore (FederationConfigStore)
import Wire.FederationConfigStore.Cassandra (interpretFederationDomainConfig, remotesMapFromCfgFile)
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess.Rpc
import Wire.GundeckAPIAccess
import Wire.HashPassword
import Wire.IndexedUserStore
import Wire.IndexedUserStore.ElasticSearch
import Wire.InvitationStore (InvitationStore)
import Wire.InvitationStore.Cassandra (interpretInvitationStoreToCassandra)
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
import Wire.Sem.Metrics
import Wire.Sem.Metrics.IO (runMetricsToIO)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.Sem.Random
import Wire.Sem.Random.IO
import Wire.SessionStore
import Wire.SessionStore.Cassandra (interpretSessionStoreCassandra)
import Wire.TeamInvitationSubsystem
import Wire.TeamInvitationSubsystem.Error
import Wire.TeamInvitationSubsystem.Interpreter
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
     TeamInvitationSubsystem,
     UserSubsystem
   ]
    `Append` BrigLowerLevelEffects

-- | These effects have interpreters which don't depend on each other
type BrigLowerLevelEffects =
  '[ EmailSubsystem,
     VerificationCodeSubsystem,
     PropertySubsystem,
     DeleteQueue,
     Wire.Events.Events,
     Error UserSubsystemError,
     Error TeamInvitationSubsystemError,
     Error AuthenticationSubsystemError,
     Error Wire.API.Federation.Error.FederationError,
     Error VerificationCodeSubsystemError,
     Error PropertySubsystemError,
     Error HttpError,
     Wire.FederationAPIAccess.FederationAPIAccess Wire.API.Federation.Client.FederatorClient,
     HashPassword,
     UserKeyStore,
     UserStore,
     IndexedUserStore,
     SessionStore,
     PasswordStore,
     VerificationCodeStore,
     ActivationCodeStore,
     InvitationStore,
     PropertyStore,
     SFT,
     ConnectionStore InternalPaging,
     Input VerificationCodeThrottleTTL,
     Input UTCTime,
     Input (Local ()),
     Input (Maybe AllowlistEmailDomains),
     Input TeamTemplates,
     Input (Maybe PasswordHashingOptions),
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
     Metrics,
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
          { emailVisibilityConfig = e.settings.emailVisibility,
            defaultLocale = Opt.defaultUserLocale e.settings,
            searchSameTeamOnly = fromMaybe False e.settings.searchSameTeamOnly,
            maxTeamSize = e.settings.maxTeamSize
          }
      teamInvitationSubsystemConfig =
        TeamInvitationSubsystemConfig
          { maxTeamSize = e.settings.maxTeamSize,
            teamInvitationTimeout = e.settings.teamInvitationTimeout
          }
      federationApiAccessConfig =
        FederationAPIAccessConfig
          { ownDomain = e.settings.federationDomain,
            federatorEndpoint = e.federator,
            http2Manager = e.http2Manager,
            requestId = e.requestId
          }
      propertySubsystemConfig =
        PropertySubsystemConfig
          { maxKeyLength = fromMaybe Opt.defMaxKeyLen e.settings.propertyMaxKeyLen,
            maxValueLength = fromMaybe Opt.defMaxValueLen e.settings.propertyMaxValueLen,
            maxProperties = 16
          }
      mainESEnv = e.indexEnv ^. to idxElastic
      indexedUserStoreConfig =
        IndexedUserStoreConfig
          { conn =
              ESConn
                { env = mainESEnv,
                  indexName = e.indexEnv ^. to idxName
                },
            additionalConn =
              (e.indexEnv ^. to idxAdditionalName) <&> \additionalIndexName ->
                ESConn
                  { env = e.indexEnv ^. to idxAdditionalElastic . to (fromMaybe mainESEnv),
                    indexName = additionalIndexName
                  }
          }

      -- These interpreters depend on each other, we use let recursion to solve that.
      userSubsystemInterpreter :: (Members BrigLowerLevelEffects r) => InterpreterFor UserSubsystem r
      userSubsystemInterpreter = runUserSubsystem userSubsystemConfig authSubsystemInterpreter

      authSubsystemInterpreter :: (Members BrigLowerLevelEffects r) => InterpreterFor AuthenticationSubsystem r
      authSubsystemInterpreter = interpretAuthenticationSubsystem userSubsystemInterpreter

  ( either throwM pure
      <=< ( runFinal
              . unsafelyPerformConcurrency
              . asyncToIOFinal
              . interpretRace
              . embedToFinal
              . runEmbedded (runHttpClientIO e)
              . loggerToTinyLogReqId e.requestId e.appLogger
              . runError @SomeException
              . mapError @ErrorCall SomeException
              . mapError @ParseException SomeException
              . interpretClientToIO e.casClient
              . runMetricsToIO
              . runRpcWithHttp e.httpManager e.requestId
              . emailSendingInterpreter e
              . interpretGalleyAPIAccessToRpc e.disabledVersions e.galleyEndpoint
              . passwordResetCodeStoreToCassandra @Cas.Client
              . randomToIO
              . runDelay
              . nowToIOAction e.currentTime
              . userPendingActivationStoreToCassandra
              . interpretBlockListStoreToCassandra e.casClient
              . interpretJwtTools
              . interpretPublicKeyBundle
              . interpretJwk
              . interpretFederationDomainConfig e.casClient e.settings.federationStrategy (foldMap (remotesMapFromCfgFile . fmap (.federationDomainConfig)) e.settings.federationDomainConfigs)
              . runGundeckAPIAccess e.gundeckEndpoint
              . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig e.requestId)
              . runInputConst e.settings.passwordHashingOptions
              . runInputConst (teamTemplatesNoLocale e)
              . runInputConst e.settings.allowlistEmailDomains
              . runInputConst (toLocalUnsafe e.settings.federationDomain ())
              . runInputSem (embed getCurrentTime)
              . runInputConst (fromIntegral $ Opt.twoFACodeGenerationDelaySecs e.settings)
              . connectionStoreToCassandra
              . interpretSFT e.httpManager
              . interpretPropertyStoreCassandra e.casClient
              . interpretInvitationStoreToCassandra e.casClient
              . interpretActivationCodeStoreToCassandra e.casClient
              . interpretVerificationCodeStoreCassandra e.casClient
              . interpretPasswordStore e.casClient
              . interpretSessionStoreCassandra e.casClient
              . interpretIndexedUserStoreES indexedUserStoreConfig
              . interpretUserStoreCassandra e.casClient
              . interpretUserKeyStoreCassandra e.casClient
              . runHashPassword
              . interpretFederationAPIAccess federationApiAccessConfig
              . rethrowHttpErrorIO
              . mapError propertySubsystemErrorToHttpError
              . mapError verificationCodeSubsystemErrorToHttpError
              . mapError (StdError . federationErrorToWai)
              . mapError authenticationSubsystemErrorToHttpError
              . mapError teamInvitationErrorToHttpError
              . mapError userSubsystemErrorToHttpError
              . runEvents
              . runDeleteQueue e.internalEvents
              . interpretPropertySubsystem propertySubsystemConfig
              . interpretVerificationCodeSubsystem
              . emailSubsystemInterpreter e.userTemplates e.teamTemplates e.templateBranding
              . userSubsystemInterpreter
              . runTeamInvitationSubsystem teamInvitationSubsystemConfig
              . authSubsystemInterpreter
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
  case e.smtpEnv of
    Just smtp -> emailViaSMTPInterpreter e.appLogger smtp
    Nothing -> emailViaSESInterpreter (e.awsEnv ^. amazonkaEnv)
