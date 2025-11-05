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

module Wire.Sem.Random.Null
  ( randomToNull,
  )
where

import Crypto.Random
import Data.Id (Id (..))
import qualified Data.UUID as UUID
import Imports
import Polysemy
import Wire.Sem.Random (Random (..))

randomToNull ::
  Sem (Random ': r) a ->
  Sem r a
randomToNull = interpret $ \case
  Bytes i -> pure $ mconcat $ replicate i "0"
  Uuid -> pure UUID.nil
  NewId -> pure $ Id UUID.nil
  ScimTokenId -> pure $ Id UUID.nil
  LiftRandom m -> pure $ fst $ withDRG (drgNewSeed $ seedFromInteger 0) m
  NDigitNumber n -> pure $ 10 ^ n
