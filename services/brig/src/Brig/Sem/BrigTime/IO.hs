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

module Brig.Sem.BrigTime.IO (brigTimeToIO) where

import Brig.Sem.BrigTime
import Data.Time
import Imports
import Polysemy

-- | An interpreter of the 'BrigTime' effect to IO.
brigTimeToIO ::
  forall m r a.
  (MonadIO m, Member (Embed m) r) =>
  IO UTCTime ->
  Sem (BrigTime ': r) a ->
  Sem r a
brigTimeToIO ioTime =
  interpret $ \Get -> embed @m $ liftIO ioTime
