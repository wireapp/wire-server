module Brig.InternalEvent.Publish where

import Brig.App
import Brig.InternalEvent.Types
import Brig.Stomp                     as Stomp
import Control.Lens (view)

publish :: InternalNotification -> AppIO ()
publish n = view stompEnv >>= \env ->
    Stomp.enqueue (broker env) (internalQueue env) n
