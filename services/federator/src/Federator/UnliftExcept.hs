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

-- | This instance defines an instance of MonadUnliftIO for (ExceptT). This was
-- copied from https://github.com/fpco/unliftio/issues/68.
--
-- This is required if we want to do any actions requiring MonadUnliftIO in the
-- 'Federator' monad, which is defined as 'AppT ServerErrorIO'. Here
-- 'ServerErrorIO' is an alias for 'ExceptT ServerError IO'.
--
-- This raises a few questions:
--
-- Q 1. Why is the Federator monad not defined as `AppT IO`?
--
-- Servers defined using mu-rpc must be defined in terms of an m which is bound
-- by this constraint: (MonadError ServerError m, MonadIO m). To be able to
-- define an instance of 'MonadError ServerError', we decided to defined the
-- 'Federator' monad as 'AppT ServerErrorIO' and not 'AppT ServerError'.
--
-- Q 2. What actions requiring MonadUnliftIO do we have to perform and where?
--
-- We decided to use Bilge to make calls to components from the federator. All
-- the ways to call components in Bilge require MonadUnliftIO. Maybe this can be
-- circumvented by adding more functions to Bilge, but we didn't analyze this
-- yet.
--
-- Q 3. This solution isn't accepted upstream, does it work?
--
-- We haven't gotten around to verifying this yet. It works in happy paths, but
-- we don't know if it will work when async exceptions are raised.
--
-- FUTUREWORK: Verify whether we can make calls to components without needing
-- MonadUnliftIO.
--
-- FUTUREWORK: Verify wheter this instance works when async exceptions are
-- raised.
instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
  withRunInIO exceptToIO = ExceptT $
    try $ do
      withRunInIO $ \runInIO ->
        exceptToIO (runInIO . (either throwIO pure <=< runExceptT))
