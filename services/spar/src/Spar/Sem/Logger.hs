module Spar.Sem.Logger
  ( module Spar.Sem.Logger
  , SAML.Level (..)
  ) where

import Polysemy
import qualified SAML2.WebSSO as SAML
import Imports hiding (log)

data Logger msg m a where
  Log :: SAML.Level -> msg -> Logger msg m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''Logger


mapLogger
  :: forall msg msg' r a
   . Member (Logger msg') r
  => (msg -> msg') -> Sem (Logger msg ': r) a -> Sem r a
mapLogger f = interpret $ \case
  Log lvl msg -> log lvl $ f msg

trace :: Member (Logger msg) r => msg -> Sem r ()
trace = log SAML.Trace

debug :: Member (Logger msg) r => msg -> Sem r ()
debug = log SAML.Debug

info :: Member (Logger msg) r => msg -> Sem r ()
info = log SAML.Info

warn :: Member (Logger msg) r => msg -> Sem r ()
warn = log SAML.Warn

-- err :: Member (Logger msg) r => msg -> Sem r ()
-- err = log SAML.Error

fatal :: Member (Logger msg) r => msg -> Sem r ()
fatal = log SAML.Fatal

