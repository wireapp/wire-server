{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Federator.UnliftExcept where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Imports
import UnliftIO.Exception

-- TODO: Check if this is actually sane
-- TODO: See if we can rid RPC of MonadUnliftIO
instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
  withRunInIO exceptToIO = ExceptT $
    try $ do
      withRunInIO $ \runInIO ->
        exceptToIO (runInIO . (either throwIO pure <=< runExceptT))
