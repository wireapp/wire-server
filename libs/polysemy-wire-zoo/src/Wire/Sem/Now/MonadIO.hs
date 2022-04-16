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

module Wire.Sem.Now.MonadIO
  ( nowToMonadIOAction,
    nowToMonadIO,
  )
where

import Data.Time
import Imports
import Polysemy
import Wire.Sem.FromUTC
import Wire.Sem.Now

-- | An interpreter of the 'Now' effect to MonadIO via a custom IO action that
-- provides the current time.
nowToMonadIOAction ::
  forall m r a.
  (MonadIO m, Member (Embed m) r) =>
  IO UTCTime ->
  Sem (Now ': r) a ->
  Sem r a
nowToMonadIOAction ioTime = interpret now
  where
    now ::
      forall x (rInitial :: EffectRow).
      Now (Sem rInitial) x ->
      Sem r x
    now Get = embed @m $ fromUTCTime @x <$> liftIO ioTime

-- | A specialisation of 'nowToMonadIOAction' to the 'getCurrentTime' IO action.
nowToMonadIO ::
  forall m r a.
  (MonadIO m, Member (Embed m) r) =>
  Sem (Now ': r) a ->
  Sem r a
nowToMonadIO = nowToMonadIOAction getCurrentTime
