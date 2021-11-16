-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Effects.WaiRoutes.IO where

import Control.Error
import qualified Data.ProtocolBuffers as Proto
import Data.Serialize.Get
import Galley.API.Error
import Galley.Effects.WaiRoutes
import Imports
import Network.Wai.Utilities hiding (Error)
import Polysemy
import Polysemy.Error

interpretWaiRoutes ::
  Members '[Embed IO, Error InvalidInput] r =>
  Sem (WaiRoutes ': r) a ->
  Sem r a
interpretWaiRoutes = interpret $ \case
  FromJsonBody r -> exceptT (throw . InvalidPayload) return (parseBody r)
  FromOptionalJsonBody r -> exceptT (throw . InvalidPayload) return (parseOptionalBody r)
  FromProtoBody r -> do
    b <- readBody r
    either (throw . InvalidPayload . fromString) return (runGetLazy Proto.decodeMessage b)
