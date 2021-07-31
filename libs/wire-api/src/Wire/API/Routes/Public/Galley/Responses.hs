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
import Wire.API.ErrorDescription (ErrorDescription (..), RespondWithErrorDescription, convNotFound)
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Routes.MultiVerb (AsUnion (..), Respond, RespondEmpty)

data RemoveFromConversation
  = RemoveFromConversationUnchanged
  | RemoveFromConversationUpdated Public.Event
  | RemoveFromConversationNotAllowed
  | RemoveFromConversationNotFound

type RemoveFromConversationResponses =
  '[ RespondEmpty 204 "No change",
     Respond 200 "Member removed" Public.Event,
     RespondWithErrorDescription 403 "invalid-op" "Conversation type does not allow removing members",
     RespondWithErrorDescription 404 "no-conversation" "Conversation not found"
   ]

instance AsUnion RemoveFromConversationResponses RemoveFromConversation where
  toUnion RemoveFromConversationUnchanged = Z (I ())
  toUnion (RemoveFromConversationUpdated e) = S (Z (I e))
  toUnion RemoveFromConversationNotAllowed = S (S (Z (I (ErrorDescription "Conversation type does not allow removing members"))))
  toUnion RemoveFromConversationNotFound = S (S (S (Z (I convNotFound))))

  fromUnion (Z (I ())) = RemoveFromConversationUnchanged
  fromUnion (S (Z (I e))) = RemoveFromConversationUpdated e
  fromUnion (S (S (Z _))) = RemoveFromConversationNotAllowed
  fromUnion (S (S (S (Z _)))) = RemoveFromConversationNotFound
  fromUnion (S (S (S (S x)))) = case x of
