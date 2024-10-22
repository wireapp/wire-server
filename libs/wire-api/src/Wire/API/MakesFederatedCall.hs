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
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Wire.API.MakesFederatedCall
  ( Component (..),
    ShowComponent,
  )
where

import Data.Aeson
import Data.Schema
import GHC.TypeLits
import Imports
import Servant.API
import Test.QuickCheck (Arbitrary)
import Wire.Arbitrary (GenericUniform (..))

data Component
  = Brig
  | Galley
  | Cargohold
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform Component)
  deriving (ToJSON, FromJSON) via (Schema Component)

instance ToSchema Component where
  schema =
    enum @Text "Component" $
      mconcat
        [ element "brig" Brig,
          element "galley" Galley,
          element "cargohold" Cargohold
        ]

instance FromHttpApiData Component where
  parseUrlPiece :: Text -> Either Text Component
  parseUrlPiece = \case
    "brig" -> Right Brig
    "galley" -> Right Galley
    "cargohold" -> Right Cargohold
    c -> Left $ "Invalid component: " <> c

instance ToHttpApiData Component where
  toUrlPiece = \case
    Brig -> "brig"
    Galley -> "galley"
    Cargohold -> "cargohold"

-- | Get a symbol representation of our component.
type family ShowComponent (x :: Component) = (res :: Symbol) | res -> x where
  ShowComponent 'Brig = "brig"
  ShowComponent 'Galley = "galley"
  ShowComponent 'Cargohold = "cargohold"
