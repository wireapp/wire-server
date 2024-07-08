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

module Wire.Sem.Random
  ( Random (..),
    bytes,
    uuid,
    scimTokenId,
    liftRandom,
    nDigitNumber,
  )
where

import Crypto.Random.Types
import Data.Id (ScimTokenId)
import Data.UUID (UUID)
import Imports
import Polysemy

data Random m a where
  Bytes :: Int -> Random m ByteString
  Uuid :: Random m UUID
  ScimTokenId :: Random m ScimTokenId
  LiftRandom :: (forall mr. (MonadRandom mr) => mr a) -> Random m a
  NDigitNumber :: Int -> Random m Integer

makeSem ''Random
