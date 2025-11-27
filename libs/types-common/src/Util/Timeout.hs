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

module Util.Timeout
  ( Timeout (..),
    module Data.Time.Clock,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Time.Clock
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), choose)

newtype Timeout = Timeout
  { timeoutDiff :: NominalDiffTime
  }
  deriving newtype (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Arbitrary Timeout where
  arbitrary = Timeout . fromIntegral <$> choose (60 :: Int, 10 * 24 * 3600)

instance Read Timeout where
  readsPrec i s =
    case readsPrec i s of
      [(x :: Int, s')] -> [(Timeout (fromIntegral x), s')]
      _ -> []

instance FromJSON Timeout where
  parseJSON (Number n) =
    let defaultV = 3600
        bounded = toBoundedInteger n :: Maybe Int64
     in pure $
          Timeout $
            fromIntegral @Int $
              maybe defaultV fromIntegral bounded
  parseJSON v = typeMismatch "Timeout" v
