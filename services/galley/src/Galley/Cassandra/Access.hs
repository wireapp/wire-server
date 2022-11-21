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

module Galley.Cassandra.Access where

import Cassandra
import Galley.Data.Conversation
import Imports hiding (Set)
import Wire.API.Conversation hiding (Conversation)

defAccess :: ConvType -> Maybe (Set Access) -> [Access]
defAccess SelfConv Nothing = [PrivateAccess]
defAccess ConnectConv Nothing = [PrivateAccess]
defAccess One2OneConv Nothing = [PrivateAccess]
defAccess RegularConv Nothing = defRegularConvAccess
defAccess SelfConv (Just (Set [])) = [PrivateAccess]
defAccess ConnectConv (Just (Set [])) = [PrivateAccess]
defAccess One2OneConv (Just (Set [])) = [PrivateAccess]
defAccess RegularConv (Just (Set [])) = defRegularConvAccess
defAccess GlobalTeamConv s = maybe [SelfInviteAccess] fromSet s
defAccess _ (Just (Set (x : xs))) = x : xs

privateOnly :: Set Access
privateOnly = Set [PrivateAccess]
