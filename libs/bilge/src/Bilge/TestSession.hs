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

module Bilge.TestSession where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.State (StateT)
import Control.Monad.State qualified as ST
import Imports
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Network.Wai.Test.Internal qualified as WaiTest

newtype SessionT m a = SessionT {unSessionT :: ReaderT Wai.Application (StateT WaiTest.ClientState m) a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail)

instance MonadTrans SessionT where
  lift = SessionT . lift . lift

liftSession :: (MonadIO m) => WaiTest.Session a -> SessionT m a
liftSession session = SessionT $ do
  app <- ask
  clientState <- lift ST.get
  let resultInState = runReaderT session app
  let resultInIO = ST.evalStateT resultInState clientState
  liftIO resultInIO
