module Spar.Sem.BrigAccess.Http where

import Spar.Sem.BrigAccess
import Bilge
import Polysemy
import Imports

brigAccessToHttp :: Member (Embed IO) r => Bilge.Manager -> Bilge.Request -> Sem (BrigAccess ': r) a -> Sem r a
brigAccessToHttp mgr req = interpret $ \case
  Call modreq ->
    embed @IO $ runReaderT (unwrap $ httpLbs req modreq) mgr

