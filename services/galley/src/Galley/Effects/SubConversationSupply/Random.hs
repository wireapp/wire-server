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

module Galley.Effects.SubConversationSupply.Random
  ( interpretSubConversationSupplyToRandom,
  )
where

import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)
import Galley.Effects.SubConversationSupply
import Imports
import Polysemy
import Wire.API.MLS.Group
import Wire.Sem.Random

interpretSubConversationSupplyToRandom ::
  Member Random r =>
  Sem (SubConversationSupply ': r) a ->
  Sem r a
interpretSubConversationSupplyToRandom = interpret $ \case
  MakeFreshGroupId -> freshGroupId

freshGroupId :: Member Random r => Sem r GroupId
freshGroupId =
  GroupId . convert . Crypto.hash @ByteString @Crypto.SHA256 <$> bytes 100
