-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    fromConvSize,
    fromMemberSize,
    ConvSizeChecked,
    ConvMemberAddSizeChecked,
    checkedConvSize,
    checkedMemberAddSize,
  )
where

import Control.Lens
import Control.Monad.Catch
import Data.List1 (List1, list1)
import Data.Range
import Galley.API.Error
import Galley.App
import Galley.Options
import Imports

rangeChecked :: Within a n m => a -> Galley (Range n m a)
rangeChecked = either throwErr return . checkedEither
{-# INLINE rangeChecked #-}

rangeCheckedMaybe :: Within a n m => Maybe a -> Galley (Maybe (Range n m a))
rangeCheckedMaybe Nothing = return Nothing
rangeCheckedMaybe (Just a) = Just <$> rangeChecked a
{-# INLINE rangeCheckedMaybe #-}

-- Between 0 and (setMaxConvSize - 1)
newtype ConvSizeChecked a = ConvSizeChecked {fromConvSize :: a}

-- Between 1 and setMaxConvSize
newtype ConvMemberAddSizeChecked a = ConvMemberAddSizeChecked {fromMemberSize :: a}

checkedConvSize :: Bounds a => a -> Galley (ConvSizeChecked a)
checkedConvSize x = do
  o <- view options
  let minV :: Integer = 0
      limit = o ^. optSettings . setMaxConvSize - 1
  if within x minV (fromIntegral limit)
    then return (ConvSizeChecked x)
    else throwErr (errorMsg minV limit "")

checkedMemberAddSize :: [a] -> Galley (ConvMemberAddSizeChecked (List1 a))
checkedMemberAddSize [] = throwErr "List must be of at least size 1"
checkedMemberAddSize l@(x : xs) = do
  o <- view options
  let minV :: Integer = 1
      limit = (o ^. optSettings . setMaxConvSize)
  if within l minV (fromIntegral limit)
    then return (ConvMemberAddSizeChecked $ list1 x xs)
    else throwErr (errorMsg minV limit "")

throwErr :: String -> Galley a
throwErr = throwM . invalidRange . fromString
