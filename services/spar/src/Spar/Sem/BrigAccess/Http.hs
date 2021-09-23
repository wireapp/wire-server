module Spar.Sem.BrigAccess.Http where

import Bilge
import Imports
import Polysemy
import Spar.Sem.BrigAccess

brigAccessToHttp :: Member (Embed IO) r => Bilge.Manager -> Bilge.Request -> Sem (BrigAccess ': r) a -> Sem r a
brigAccessToHttp mgr req = interpret $ \case
  Call modreq ->
    embed @IO $ runReaderT (unwrap $ httpLbs req modreq) mgr

