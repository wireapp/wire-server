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

module Wire.API.Federation.Component
  ( module Wire.API.Federation.Component,
    Component (..),
  )
where

import Data.Proxy
import Imports
import Wire.API.MakesFederatedCall (Component (..))

componentName :: Component -> Text
componentName Brig = "brig"
componentName Galley = "galley"
componentName Cargohold = "cargohold"

class KnownComponent (c :: Component) where
  componentVal :: Component

instance KnownComponent 'Brig where
  componentVal = Brig

instance KnownComponent 'Galley where
  componentVal = Galley

instance KnownComponent 'Cargohold where
  componentVal = Cargohold

data SomeComponent where
  SomeComponent :: (KnownComponent c) => Proxy c -> SomeComponent

someComponent :: Component -> SomeComponent
someComponent Brig = SomeComponent (Proxy @'Brig)
someComponent Galley = SomeComponent (Proxy @'Galley)
someComponent Cargohold = SomeComponent (Proxy @'Cargohold)
