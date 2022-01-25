{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Spar.Sem.GalleyAccess.Http (
  RunHttp(..),
  RunHttpEnv(..),
  semToRunHttp,
  viaRunHttp,
  galleyAccessToHttp ) where

import Bilge
import Control.Monad.Except
import Imports hiding (log)
import Polysemy
import Polysemy.Error
import Spar.Error (SparError)
import Spar.Intra.Brig (MonadSparToBrig (..))
import Spar.Intra.Galley (MonadSparToGalley)
import qualified Spar.Intra.Galley as Intra
import Spar.Sem.GalleyAccess
import Spar.Sem.Logger (Logger)
import qualified Spar.Sem.Logger as Logger
import Spar.Sem.Logger.TinyLog (fromLevel)
import qualified System.Logger as TinyLog
import qualified System.Logger.Class as TinyLog

data RunHttpEnv r = RunHttpEnv
  { rheManager :: Bilge.Manager,
    rheRequest :: Bilge.Request
  }

newtype RunHttp r a = RunHttp
  { unRunHttp :: ReaderT (RunHttpEnv r) (ExceptT SparError (HttpT (Sem r))) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError SparError, MonadReader (RunHttpEnv r))

instance Member (Embed IO) r => MonadIO (RunHttp r) where
  liftIO = semToRunHttp . embed

instance Member (Embed IO) r => MonadHttp (RunHttp r) where
  handleRequestWithCont r fribia =
    RunHttp $
      lift $
        lift $
          handleRequestWithCont r fribia

semToRunHttp :: Sem r a -> RunHttp r a
semToRunHttp = RunHttp . lift . lift . lift

viaRunHttp ::
  Members '[Error SparError, Embed IO] r =>
  RunHttpEnv r ->
  RunHttp r a ->
  Sem r a
viaRunHttp env m = do
  ma <- runHttpT (rheManager env) $ runExceptT $ flip runReaderT env $ unRunHttp m
  case ma of
    Left err -> throw err
    Right a -> pure a

instance Member (Logger (TinyLog.Msg -> TinyLog.Msg)) r => TinyLog.MonadLogger (RunHttp r) where
  log lvl msg = semToRunHttp $ Logger.log (fromLevel lvl) msg

instance Members '[Logger (TinyLog.Msg -> TinyLog.Msg), Embed IO] r => MonadSparToGalley (RunHttp r) where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

instance Members '[Logger (TinyLog.Msg -> TinyLog.Msg), Embed IO] r => MonadSparToBrig (RunHttp r) where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

galleyAccessToHttp ::
  Members '[Logger (TinyLog.Msg -> TinyLog.Msg), Error SparError, Embed IO] r =>
  Bilge.Manager ->
  Bilge.Request ->
  Sem (GalleyAccess ': r) a ->
  Sem r a
galleyAccessToHttp mgr req =
  interpret $
    viaRunHttp (RunHttpEnv mgr req) . \case
      GetTeamMembers itlt -> Intra.getTeamMembers itlt
      AssertHasPermission itlt perm itlu -> Intra.assertHasPermission itlt perm itlu
      AssertSSOEnabled itlt -> Intra.assertSSOEnabled itlt
      IsEmailValidationEnabledTeam itlt -> Intra.isEmailValidationEnabledTeam itlt
