module Brig.CanonicalInterpreter where

import Brig.App
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore.Cassandra (interpretBlacklistPhonePrefixStoreToCassandra)
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore.Cassandra (interpretBlacklistStoreToCassandra)
import Brig.Effects.CodeStore (CodeStore)
import Brig.Effects.CodeStore.Cassandra (codeStoreToCassandra, interpretClientToIO)
import Brig.Effects.JwtTools
import Brig.Effects.PasswordResetStore (PasswordResetStore)
import Brig.Effects.PasswordResetStore.CodeStore (passwordResetStoreToCodeStore)
import Brig.Effects.PublicKeyBundle
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore.Cassandra (userPendingActivationStoreToCassandra)
import qualified Cassandra as Cas
import Control.Lens ((^.))
import Imports
import Polysemy (Embed, Final, embedToFinal, runFinal)
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO (nowToIOAction)
import Wire.Sem.Paging.Cassandra (InternalPaging)

type BrigCanonicalEffects =
  '[ PublicKeyBundle,
     JwtTools,
     BlacklistPhonePrefixStore,
     BlacklistStore,
     PasswordResetStore,
     UserPendingActivationStore InternalPaging,
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
    . userPendingActivationStoreToCassandra
    . passwordResetStoreToCodeStore
    . interpretBlacklistStoreToCassandra @Cas.Client
    . interpretBlacklistPhonePrefixStoreToCassandra @Cas.Client
    . interpretJwtTools
    . interpretPublicKeyBundle
    $ runReaderT ma e
