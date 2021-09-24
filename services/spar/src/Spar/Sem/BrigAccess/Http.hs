module Spar.Sem.BrigAccess.Http where

import Bilge
import Imports
import Polysemy
import Polysemy.Error (Error)
import Spar.Error (SparError)
import qualified Spar.Intra.Brig as Intra
import Spar.Sem.BrigAccess
import Spar.Sem.GalleyAccess.Http (RunHttpEnv (..), viaRunHttp)

brigAccessToHttp ::
  Members '[Error SparError, Embed IO] r =>
  Bilge.Manager ->
  Bilge.Request ->
  Sem (BrigAccess ': r) a ->
  Sem r a
brigAccessToHttp mgr req =
  interpret $
    viaRunHttp (RunHttpEnv mgr req) . \case
      CreateSAML u itlu itlt n m -> Intra.createBrigUserSAML u itlu itlt n m
      CreateNoSAML e itlt n -> Intra.createBrigUserNoSAML e itlt n
      UpdateEmail itlu e -> Intra.updateEmail itlu e
      GetAccount h itlu -> Intra.getBrigUserAccount h itlu
      GetByHandle h -> Intra.getBrigUserByHandle h
      GetByEmail e -> Intra.getBrigUserByEmail e
      SetName itlu n -> Intra.setBrigUserName itlu n
      SetHandle itlu h -> Intra.setBrigUserHandle itlu h
      SetManagedBy itlu m -> Intra.setBrigUserManagedBy itlu m
      SetVeid itlu v -> Intra.setBrigUserVeid itlu v
      SetRichInfo itlu r -> Intra.setBrigUserRichInfo itlu r
      GetRichInfo itlu -> Intra.getBrigUserRichInfo itlu
      CheckHandleAvailable h -> Intra.checkHandleAvailable h
      Delete itlu -> Intra.deleteBrigUser itlu
      EnsureReAuthorised mitlu mp -> Intra.ensureReAuthorised mitlu mp
      SsoLogin itlu -> Intra.ssoLogin itlu
      GetStatus itlu -> Intra.getStatus itlu
      GetStatusMaybe itlu -> Intra.getStatusMaybe itlu
      SetStatus itlu a -> Intra.setStatus itlu a
