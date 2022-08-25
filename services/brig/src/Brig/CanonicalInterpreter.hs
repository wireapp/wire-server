module Brig.CanonicalInterpreter where

import Brig.App
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore.Cassandra (interpretBlacklistPhonePrefixStoreToCassandra)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import Brig.Sem.CodeStore (CodeStore)
import Brig.Sem.CodeStore.Cassandra (codeStoreToCassandra, interpretClientToIO)
import Brig.Sem.GalleyProvider (GalleyProvider)
import Brig.Sem.GalleyProvider.RPC (interpretGalleyProviderToRPC)
import Brig.Sem.PasswordResetStore (PasswordResetStore)
import Brig.Sem.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import Brig.Sem.RPC (RPC)
import Brig.Sem.RPC.IO (interpretRpcToIO)
import qualified Cassandra as Cas
import Control.Lens ((^.))
import Imports
import Polysemy (Embed, Final, embedToFinal, runFinal)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Brig.Sem.ServiceRPC (ServiceRPC, Service(Galley))
import Brig.Sem.ServiceRPC.IO (interpretServiceRpcToRpc)
import Polysemy.Error (runError, mapError, Error)
import Brig.RPC (ParseException)
import Control.Monad.Catch (throwM)
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Polysemy.TinyLog (TinyLog)

type BrigCanonicalEffects =
  '[ BlacklistPhonePrefixStore,
     BlacklistStore,
     PasswordResetStore,
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
     Final IO
   ]

runBrigToIO :: Env -> AppT BrigCanonicalEffects a -> IO a
runBrigToIO e (AppT ma) = do
  (either throwM pure =<<)
    . runFinal
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
    . passwordResetStoreToCodeStore
    . interpretBlacklistStoreToCassandra @Cas.Client
    . interpretBlacklistPhonePrefixStoreToCassandra @Cas.Client
    $ runReaderT ma e
