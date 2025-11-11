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

module Wire.API.Team.HardTruncationLimit where

import Data.Proxy
import Data.Range
import GHC.TypeLits
import Imports

type HardTruncationLimit = (2000 :: Nat)

hardTruncationLimit :: (Integral a) => a
hardTruncationLimit = fromIntegral $ natVal (Proxy @HardTruncationLimit)

hardTruncationLimitRange :: (Integral a, n <= HardTruncationLimit, HardTruncationLimit <= m) => Range n m a
hardTruncationLimitRange = toRange $ Proxy @HardTruncationLimit
