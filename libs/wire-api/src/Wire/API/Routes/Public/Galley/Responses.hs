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
import qualified Data.Text as T
import Imports
import Wire.API.Conversation.Role (Action)
import Wire.API.ErrorDescription
  ( ActionDenied,
    ConvNotFound,
    CustomRolesNotSupported,
    ErrorDescription (..),
    InvalidOpConnectConv,
    InvalidOpOne2OneConv,
    InvalidOpSelfConv,
    ManagedRemovalNotAllowed,
    actionDenied,
    convNotFound,
    customRolesNotSupported,
    invalidOpConnectConv,
    invalidOpOne2OneConv,
    invalidOpSelfConv,
    managedRemovalNotAllowed,
  )
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Routes.MultiVerb (AsUnion (..), Respond, RespondEmpty)

data RemoveFromConversation
  = RemoveFromConversationNotAllowed Action
  | RemoveFromConversationManagedConvNotAllowed
  | RemoveFromConversationNotFound
  | RemoveFromConversationCustomRolesNotSupported
  | RemoveFromConversationSelfConv
  | RemoveFromConversationOne2OneConv
  | RemoveFromConversationConnectConv
  | RemoveFromConversationUnchanged
  | RemoveFromConversationUpdated Public.Event

-- | These are just the "error" outcomes of the 'RemoveFromConversation' type.
-- This is needed in using ExceptT to differentiate error outcomes from an
-- outcome reflecting a change.
data RemoveFromConversationError
  = RemoveFromConversationErrorNotAllowed Action
  | RemoveFromConversationErrorManagedConvNotAllowed
  | RemoveFromConversationErrorNotFound
  | RemoveFromConversationErrorCustomRolesNotSupported
  | RemoveFromConversationErrorSelfConv
  | RemoveFromConversationErrorOne2OneConv
  | RemoveFromConversationErrorConnectConv
  | RemoveFromConversationErrorUnchanged

instance AsUnion RemoveFromConversationResponses RemoveFromConversation where
  toUnion (RemoveFromConversationNotAllowed a) = Z (I (actionDenied a))
  toUnion RemoveFromConversationManagedConvNotAllowed = S (Z (I managedRemovalNotAllowed))
  toUnion RemoveFromConversationNotFound = S (S (Z (I convNotFound)))
  toUnion RemoveFromConversationCustomRolesNotSupported = S (S (S (Z (I customRolesNotSupported))))
  toUnion RemoveFromConversationSelfConv = S (S (S (S (Z (I invalidOpSelfConv)))))
  toUnion RemoveFromConversationOne2OneConv = S (S (S (S (S (Z (I invalidOpOne2OneConv))))))
  toUnion RemoveFromConversationConnectConv = S (S (S (S (S (S (Z (I invalidOpConnectConv)))))))
  toUnion RemoveFromConversationUnchanged = S (S (S (S (S (S (S (Z (I ()))))))))
  toUnion (RemoveFromConversationUpdated e) = S (S (S (S (S (S (S (S (Z (I e)))))))))

  fromUnion (Z (I (ErrorDescription e))) = RemoveFromConversationNotAllowed . parse $ e
    where
      -- The input is from the error description given in
      -- 'Wire.API.ErrorDescription.actionDenied'.
      parse :: Text -> Action
      parse = read . T.unpack . (!! 3) . T.words
  fromUnion (S (Z _)) = RemoveFromConversationManagedConvNotAllowed
  fromUnion (S (S (Z _))) = RemoveFromConversationNotFound
  fromUnion (S (S (S (Z _)))) = RemoveFromConversationCustomRolesNotSupported
  fromUnion (S (S (S (S (Z _))))) = RemoveFromConversationSelfConv
  fromUnion (S (S (S (S (S (Z _)))))) = RemoveFromConversationOne2OneConv
  fromUnion (S (S (S (S (S (S (Z _))))))) = RemoveFromConversationConnectConv
  fromUnion (S (S (S (S (S (S (S (Z (I _))))))))) = RemoveFromConversationUnchanged
  fromUnion (S (S (S (S (S (S (S (S (Z (I e)))))))))) = RemoveFromConversationUpdated e
  fromUnion (S (S (S (S (S (S (S (S (S x))))))))) = case x of

type RemoveFromConversationResponses =
  '[ ActionDenied,
     ManagedRemovalNotAllowed,
     ConvNotFound,
     CustomRolesNotSupported,
     InvalidOpSelfConv,
     InvalidOpOne2OneConv,
     InvalidOpConnectConv,
     RespondEmpty 204 "No change",
     Respond 200 "Member removed" Public.Event
   ]
