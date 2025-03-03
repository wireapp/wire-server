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

module Wire.API.Conversation.PydioState where

import Cassandra.CQL
import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.Arbitrary

data PydioState
  = -- | Pydio is not enabled
    PydioDisabled
  | -- | Pydio is being initialised
    PydioPending
  | -- | Pydio is ready
    PydioReady
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform PydioState)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema PydioState

instance Default PydioState where
  def = PydioDisabled

instance ToSchema PydioState where
  schema =
    enum @Text "PydioState" $
      mconcat
        [ element "disabled" PydioDisabled,
          element "pending" PydioPending,
          element "ready" PydioReady
        ]

instance Cql PydioState where
  ctype = Tagged IntColumn

  toCql PydioDisabled = CqlInt 0
  toCql PydioPending = CqlInt 1
  toCql PydioReady = CqlInt 2

  fromCql (CqlInt i) = case i of
    0 -> pure PydioDisabled
    1 -> pure PydioPending
    2 -> pure PydioReady
    n -> Left $ "unexpected pydio_state: " ++ show n
  fromCql _ = Left "pydio_state: int expected"
