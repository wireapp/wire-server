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

module Galley.API.MLS.OutOfSync where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.MLS.SubConversation
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types

checkConversationOutOfSync ::
  (Member ConversationStore r) =>
  Local ConvOrSubConv ->
  Sem r Bool
checkConversationOutOfSync lConvOrSub = case tUnqualified lConvOrSub of
  SubConv _ _ -> pure False
  Conv mc -> do
    flag <- isConversationOutOfSync mc.mcId
    if flag
      then do
        pure True
      else
        pure False
