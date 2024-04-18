module Brig.CanonicalInterpreter where

import Brig.API.Error qualified as E
import Brig.App
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore.Cassandra (interpretBlacklistPhonePrefixStoreToCassandra)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.CodeStore.Cassandra (codeStoreToCassandra, interpretClientToIO)
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.ConnectionStore.Cassandra (connectionStoreToCassandra)
import Brig.Effects.FederationConfigStore (FederationConfigStore)
import Brig.Effects.FederationConfigStore.Cassandra (interpretFederationDomainConfig, remotesMapFromCfgFile)
import Brig.Effects.JwtTools
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import Brig.Effects.PublicKeyBundle
import Brig.Effects.SFT (SFT, interpretSFT)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.Options (ImplicitNoFederationRestriction (federationDomainConfig), federationDomainConfigs, federationStrategy)
import Brig.Options qualified as Opt
import Cassandra qualified as Cas
import Control.Lens (to, (^.))
import Control.Monad.Catch (throwM)
import Data.Qualified (Local, toLocalUnsafe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Imports
import Polysemy (Embed, Final, Sem, embed, embedToFinal, runFinal)
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Embed (runEmbedded)
import Polysemy.Error (Error, mapError, runError)
import Polysemy.Input (Input, runInputConst, runInputSem)
import Polysemy.TinyLog (TinyLog)
import Wire.API.Federation.Client qualified
import Wire.API.Federation.Error qualified
import Wire.FederationAPIAccess qualified
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess.Rpc
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter (defaultNotificationSubsystemConfig, runNotificationSubsystemGundeck)
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Delay
import Wire.Sem.Jwk
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.UserStore
import Wire.UserStore.Cassandra
import Wire.UserSubsystem
import Wire.UserSubsystem.Interpreter

type BrigCanonicalEffects =
  '[ UserSubsystem,
     Error Wire.API.Federation.Error.FederationError,
     Wire.FederationAPIAccess.FederationAPIAccess Wire.API.Federation.Client.FederatorClient,
     UserStore,
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
     PasswordResetStore,
     UserPendingActivationStore InternalPaging,
     Now,
     Delay,
     CodeStore,
     GalleyAPIAccess,
     Rpc,
     Embed Cas.Client,
     Error ParseException,
     Error SomeException,
     TinyLog,
     Embed HttpClientIO,
     Embed IO,
     Race,
     Async,
     Concurrency 'Unsafe,
     Final IO
   ]

runBrigToIO :: Env -> AppT BrigCanonicalEffects a -> IO a
runBrigToIO e (AppT ma) = do
  let userSubsystemConfig =
        UserSubsystemConfig
          { emailVisibilityConfig = e ^. settings . Opt.emailVisibility,
            defaultLocale = e ^. settings . to Opt.setDefaultUserLocale
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
              . mapError @ParseException SomeException
              . interpretClientToIO (e ^. casClient)
              . runRpcWithHttp (e ^. httpManager) (e ^. requestId)
              . interpretGalleyAPIAccessToRpc (e ^. disabledVersions) (e ^. galleyEndpoint)
              . codeStoreToCassandra @Cas.Client
              . runDelay
              . nowToIOAction (e ^. currentTime)
              . userPendingActivationStoreToCassandra
              . passwordResetStoreToCodeStore
              . interpretBlacklistStoreToCassandra @Cas.Client
              . interpretBlacklistPhonePrefixStoreToCassandra @Cas.Client
              . interpretJwtTools
              . interpretPublicKeyBundle
              . interpretJwk
              . interpretFederationDomainConfig (e ^. settings . federationStrategy) (foldMap (remotesMapFromCfgFile . fmap (.federationDomainConfig)) (e ^. settings . federationDomainConfigs))
              . runGundeckAPIAccess (e ^. gundeckEndpoint)
              . runNotificationSubsystemGundeck (defaultNotificationSubsystemConfig (e ^. requestId))
              . runInputConst (toLocalUnsafe (e ^. settings . Opt.federationDomain) ())
              . runInputSem (embed getCurrentTime)
              . connectionStoreToCassandra
              . interpretSFT (e ^. httpManager)
              . interpretUserStoreCassandra (e ^. casClient)
              . runFederationAPIAccess
              . throwLeftAsWaiError undefined
              . runUserSubsystem userSubsystemConfig
          )
    )
    $ runReaderT ma e

throwLeftAsWaiError :: (e -> E.Error) -> Sem (Error e ': r) a -> Sem r a
throwLeftAsWaiError = undefined

runFederationAPIAccess :: Sem (Wire.FederationAPIAccess.FederationAPIAccess Wire.API.Federation.Client.FederatorClient ': r) a -> Sem r a
runFederationAPIAccess = undefined
