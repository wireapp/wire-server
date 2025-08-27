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

module Wire.Util where

import Cassandra hiding (Set)
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.Input (Input, input)
import Polysemy.TinyLog
import System.Logger.Message

embedClient :: (Member (Embed IO) r) => ClientState -> Client x -> Sem r x
embedClient client = runEmbedded (runClient client) . embed

logEffect :: (Member TinyLog r) => ByteString -> Sem r ()
logEffect = debug . msg . val

embedClientInput :: (Member (Embed IO) r, Member (Input ClientState) r) => Client x -> Sem r x
embedClientInput a = do
  client <- input
  embedClient client a
