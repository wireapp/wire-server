module Brig.CanonicalInterpreter where

import Brig.App
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore.Cassandra (interpretBlacklistPhonePrefixStoreToCassandra)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.CodeStore.Cassandra (codeStoreToCassandra, interpretClientToIO)
import Brig.Effects.GalleyProvider (GalleyProvider)
import Brig.Effects.GalleyProvider.RPC (interpretGalleyProviderToRPC)
import Brig.Effects.JwtTools
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import Brig.Effects.PublicKeyBundle
import Brig.Effects.RPC (RPC)
import Brig.Effects.RPC.IO (interpretRpcToIO)
import Brig.Effects.ServiceRPC (Service (Galley), ServiceRPC)
import Brig.Effects.ServiceRPC.IO (interpretServiceRpcToRpc)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import Brig.RPC (ParseException)
import qualified Cassandra as Cas
import Control.Lens ((^.))
import Control.Monad.Catch (throwM)
import Imports
import Polysemy (Embed, Final, embedToFinal, runFinal)
import Polysemy.Error (Error, mapError, runError)
import Polysemy.TinyLog (TinyLog)
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Jwk
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)

type BrigCanonicalEffects =
  '[ Jwk,
     PublicKeyBundle,
     JwtTools,
     BlacklistPhonePrefixStore,
     BlacklistStore,
     PasswordResetStore,
     UserPendingActivationStore InternalPaging,
     Now,
     CodeStore,
     GalleyProvider,
     ServiceRPC 'Galley,
     RPC,
     Embed Cas.Client,
     Error ParseException,
     Error SomeException,
     TinyLog,
     Embed IO,
     Concurrency 'Unsafe,
     Final IO
   ]

runBrigToIO :: Env -> AppT BrigCanonicalEffects a -> IO a
runBrigToIO e (AppT ma) = do
  ( either throwM pure
      <=< ( runFinal
              . unsafelyPerformConcurrency
              . embedToFinal
              . loggerToTinyLog (e ^. applog)
              . runError @SomeException
              . mapError @ParseException SomeException
              . interpretClientToIO (e ^. casClient)
              . interpretRpcToIO (e ^. httpManager) (e ^. requestId)
              . interpretServiceRpcToRpc @'Galley "galley" (e ^. galley)
              . interpretGalleyProviderToRPC
              . codeStoreToCassandra @Cas.Client
              . nowToIOAction (e ^. currentTime)
              . userPendingActivationStoreToCassandra
              . passwordResetStoreToCodeStore
              . interpretBlacklistStoreToCassandra @Cas.Client
              . interpretBlacklistPhonePrefixStoreToCassandra @Cas.Client
              . interpretJwtTools
              . interpretPublicKeyBundle
              . interpretJwk
          )
    )
    $ runReaderT ma e
