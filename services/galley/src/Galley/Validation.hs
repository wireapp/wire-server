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

module Galley.Validation
  ( rangeChecked,
    rangeCheckedMaybe,
  )
where

import Data.Range
import GHC.TypeNats
import Galley.API.Error
import Imports
import Polysemy
import Polysemy.Error

rangeChecked :: (KnownNat n, KnownNat m, Member (Error InvalidInput) r, Within a n m) => a -> Sem r (Range n m a)
rangeChecked = either throwErr pure . checkedEither
{-# INLINE rangeChecked #-}

rangeCheckedMaybe ::
  (Member (Error InvalidInput) r, KnownNat n, KnownNat m, Within a n m) =>
  Maybe a ->
  Sem r (Maybe (Range n m a))
rangeCheckedMaybe Nothing = pure Nothing
rangeCheckedMaybe (Just a) = Just <$> rangeChecked a
{-# INLINE rangeCheckedMaybe #-}

throwErr :: (Member (Error InvalidInput) r) => String -> Sem r a
throwErr = throw . InvalidRange . fromString
