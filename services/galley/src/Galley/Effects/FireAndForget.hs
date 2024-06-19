{-# LANGUAGE TemplateHaskell #-}

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

module Galley.Effects.FireAndForget
  ( FireAndForget,
    fireAndForget,
    spawnMany,
    interpretFireAndForget,
  )
where

import Imports
import Polysemy
import Polysemy.Final
import UnliftIO.Async (pooledMapConcurrentlyN_)

data FireAndForget m a where
  FireAndForgetOne :: m () -> FireAndForget m ()
  SpawnMany :: [m ()] -> FireAndForget m ()

makeSem ''FireAndForget

fireAndForget :: (Member FireAndForget r) => Sem r () -> Sem r ()
fireAndForget = fireAndForgetOne

-- | Run actions in separate threads and ignore results.
--
-- /Note/: this will also ignore any state and error effects contained in the
-- 'FireAndForget' action. Use with care.
interpretFireAndForget :: (Member (Final IO) r) => Sem (FireAndForget ': r) a -> Sem r a
interpretFireAndForget = interpretFinal @IO $ \case
  FireAndForgetOne action -> do
    action' <- runS action
    liftS $ void . forkIO . void $ action'
  SpawnMany actions -> do
    actions' <- traverse runS actions
    -- I picked this number by fair dice roll, feel free to change it :P
    liftS $ pooledMapConcurrentlyN_ 8 void actions'
