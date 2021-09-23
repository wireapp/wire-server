module Spar.Sem.GalleyAccess.Http where

import Bilge
import Imports
import Polysemy
import Spar.Sem.GalleyAccess

galleyAccessToHttp :: Member (Embed IO) r => Bilge.Manager -> Bilge.Request -> Sem (GalleyAccess ': r) a -> Sem r a
galleyAccessToHttp mgr req = interpret $ \case
  Call modreq ->
    embed @IO $ runReaderT (unwrap $ httpLbs req modreq) mgr
