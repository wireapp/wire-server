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

-- | The module provides Galley HTTP response types that are shared with Galley
-- federation endpoints.
module Wire.API.Routes.Public.Galley.Responses where

import Data.SOP (I (..), NS (..))
import Imports
import Wire.API.ErrorDescription (ConvNotFound, RemovalNotAllowed, convNotFound, removalNotAllowed)
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Routes.MultiVerb (AsUnion (..), Respond, RespondEmpty)

data RemoveFromConversation
  = RemoveFromConversationNotAllowed
  | RemoveFromConversationNotFound
  | RemoveFromConversationUnchanged
  | RemoveFromConversationUpdated Public.Event
  deriving (Eq, Show)

instance AsUnion RemoveFromConversationResponses RemoveFromConversation where
  toUnion RemoveFromConversationNotAllowed = Z (I removalNotAllowed)
  toUnion RemoveFromConversationNotFound = S (Z (I convNotFound))
  toUnion RemoveFromConversationUnchanged = S (S (Z (I ())))
  toUnion (RemoveFromConversationUpdated e) = S (S (S (Z (I e))))

  fromUnion (Z _) = RemoveFromConversationNotAllowed
  fromUnion (S (Z _)) = RemoveFromConversationNotFound
  fromUnion (S (S (Z (I _)))) = RemoveFromConversationUnchanged
  fromUnion (S (S (S (Z (I e))))) = RemoveFromConversationUpdated e
  fromUnion (S (S (S (S x)))) = case x of

type RemoveFromConversationResponses =
  '[ RemovalNotAllowed,
     ConvNotFound,
     RespondEmpty 204 "No change",
     Respond 200 "Member removed" Public.Event
   ]
