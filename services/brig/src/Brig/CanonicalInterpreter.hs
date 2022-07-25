module Brig.CanonicalInterpreter where

import Brig.App
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import Brig.Sem.CodeStore (CodeStore)
import Brig.Sem.CodeStore.Cassandra (codeStoreToCassandra, interpretClientToIO)
import Brig.Sem.PasswordResetStore (PasswordResetStore)
import Brig.Sem.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import qualified Cassandra as Cas
import Control.Lens ((^.))
import Imports
import Polysemy (Embed, Final, embedToFinal, runFinal)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)

type BrigCanonicalEffects =
  '[ BlacklistStore,
     PasswordResetStore,
     Now,
     CodeStore,
     Embed Cas.Client,
     Embed IO,
     Final IO
   ]

runBrigToIO :: Env -> AppT BrigCanonicalEffects a -> IO a
runBrigToIO e (AppT ma) =
  runFinal
    . embedToFinal
    . interpretClientToIO (e ^. casClient)
    . codeStoreToCassandra @Cas.Client
    . nowToIOAction (e ^. currentTime)
    . passwordResetStoreToCodeStore
    . interpretBlacklistStoreToCassandra @Cas.Client
    $ runReaderT ma e
