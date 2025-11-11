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
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Wire.Sem.Now.IO
  ( nowToIOAction,
    nowToIO,
  )
where

import Data.Time
import Imports
import Polysemy
import Wire.Sem.Now

-- | An interpreter of the 'Now' effect to IO via a custom IO action that
-- provides the current time.
nowToIOAction ::
  forall r a.
  (Member (Embed IO) r) =>
  IO UTCTime ->
  Sem (Now ': r) a ->
  Sem r a
nowToIOAction ioTime = interpret $ \case Get -> embed @IO ioTime

-- | A specialisation of 'nowToIOAction' to the 'getCurrentTime' IO action.
nowToIO ::
  forall r a.
  (Member (Embed IO) r) =>
  Sem (Now ': r) a ->
  Sem r a
nowToIO = nowToIOAction getCurrentTime
