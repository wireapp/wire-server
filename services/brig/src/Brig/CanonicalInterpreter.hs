module Brig.CanonicalInterpreter where

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
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.GalleyProvider.RPC
import Brig.Effects.JwtTools
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import Brig.Effects.PublicKeyBundle
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.Options (ImplicitNoFederationRestriction (federationDomainConfig), federationDomainConfigs, federationStrategy)
import Brig.Options qualified as Opt
import Brig.RPC (ParseException)
import Cassandra qualified as Cas
import Control.Lens ((^.))
import Control.Monad.Catch (throwM)
import Data.Qualified (Local, toLocalUnsafe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Imports
import Polysemy (Embed, Final, embed, embedToFinal, runFinal)
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Embed (runEmbedded)
import Polysemy.Error (Error, mapError, runError)
import Polysemy.Input (Input, runInputConst, runInputSem)
import Polysemy.TinyLog (TinyLog)
import Wire.GundeckAPIAccess
import Wire.NotificationSubsystem
import Wire.NotificationSubsystem.Interpreter (defaultNotificationSubsystemConfig, runNotificationSubsystemGundeck)
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Delay
import Wire.Sem.Jwk
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)

type BrigCanonicalEffects =
  '[ ConnectionStore InternalPaging,
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
     GalleyProvider,
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
              . interpretGalleyProviderToRpc (e ^. disabledVersions) (e ^. galleyEndpoint)
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
          )
    )
    $ runReaderT ma e
