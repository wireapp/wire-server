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

module Wire.API.Conversation.CellsState where

import Cassandra.CQL
import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary

data CellsState
  = -- | Cells is not enabled
    CellsDisabled
  | -- | Cells is being initialised
    CellsPending
  | -- | Cells is ready
    CellsReady
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CellsState)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CellsState

instance Default CellsState where
  def = CellsDisabled

instance ToSchema CellsState where
  schema =
    enum @Text "CellsState" $
      mconcat
        [ element "disabled" CellsDisabled,
          element "pending" CellsPending,
          element "ready" CellsReady
        ]

instance Cql CellsState where
  ctype = Tagged IntColumn

  toCql CellsDisabled = CqlInt 0
  toCql CellsPending = CqlInt 1
  toCql CellsReady = CqlInt 2

  fromCql (CqlInt i) = case i of
    0 -> pure CellsDisabled
    1 -> pure CellsPending
    2 -> pure CellsReady
    n -> Left $ "unexpected cells_state: " ++ show n
  fromCql _ = Left "cells_state: int expected"
