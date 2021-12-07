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

module Wire.API.Federation.Component where

import Imports
import Test.QuickCheck (Arbitrary)
import Wire.API.Arbitrary (GenericUniform (..))

data Component
  = Brig
  | Galley
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform Component)

parseComponent :: Text -> Maybe Component
parseComponent "brig" = Just Brig
parseComponent "galley" = Just Galley
parseComponent _ = Nothing

componentName :: Component -> Text
componentName Brig = "brig"
componentName Galley = "galley"

class KnownComponent (c :: Component) where
  componentVal :: Component

instance KnownComponent 'Brig where
  componentVal = Brig

instance KnownComponent 'Galley where
  componentVal = Galley
