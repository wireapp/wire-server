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

module Wire.Sem.Random.IO
  ( randomToIO,
  )
where

import Data.Id (randomId)
import qualified Data.UUID.V4 as UUID
import Imports
import OpenSSL.Random (randBytes)
import Polysemy
import Wire.Sem.Random (Random (..))

randomToIO ::
  (Member (Embed IO) r) =>
  Sem (Random ': r) a ->
  Sem r a
randomToIO = interpret $ \case
  Bytes i -> embed $ randBytes i
  Uuid -> embed $ UUID.nextRandom
  ScimTokenId -> embed $ randomId @IO
  LiftRandom m -> embed @IO $ m
