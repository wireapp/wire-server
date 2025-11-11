-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.Sem.Concurrency.IO where

import Imports
import Polysemy
import Polysemy.Final
import UnliftIO (pooledMapConcurrentlyN, pooledMapConcurrentlyN_)
import Wire.Sem.Concurrency (Concurrency (..), ConcurrencySafety (Safe))

------------------------------------------------------------------------------

-- | Safely perform concurrency that wraps only IO effects.
performConcurrency ::
  (Member (Final IO) r) =>
  Sem (Concurrency 'Safe ': r) a ->
  Sem r a
performConcurrency = unsafelyPerformConcurrency

------------------------------------------------------------------------------

-- | VERY UNSAFELY perform concurrency in Polysemy. This is likely to lead to
-- obscure bugs. See the notes on 'Concurrency' to get a better understanding
-- of what can go wrong here.
unsafelyPerformConcurrency ::
  (Member (Final IO) r) =>
  Sem (Concurrency safe ': r) a ->
  Sem r a
unsafelyPerformConcurrency = interpretFinal @IO $ \case
  UnsafePooledMapConcurrentlyN n f t -> do
    st <- getInitialStateS
    faction <- bindS f
    let action a = faction $ a <$ st
    z <- liftS $ pooledMapConcurrentlyN n action $ toList t
    Inspector ins <- getInspectorS
    pure $ fmap (fmap (mapMaybe ins)) z
  UnsafePooledMapConcurrentlyN_ n f t -> do
    st <- getInitialStateS
    faction <- bindS f
    let action a = faction $ a <$ st
    liftS $ pooledMapConcurrentlyN_ n action $ toList t
