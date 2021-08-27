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

-- | The module provides Galley HTTP response types and corresponding handler
-- types.
module Wire.API.Routes.Public.Galley.Responses where

import Data.Aeson (FromJSON, ToJSON)
import Data.SOP (I (..), NS (..), unI, unZ)
import qualified Generics.SOP as GSOP
import Imports
import Servant (type (.++))
import Wire.API.ErrorDescription
  ( ConvMemberRemovalDenied,
    ConvNotFound,
    CustomRolesNotSupported,
    InvalidOpConnectConv,
    InvalidOpOne2OneConv,
    InvalidOpSelfConv,
    ManagedRemovalNotAllowed,
  )
import qualified Wire.API.Event.Conversation as Public
import Wire.API.Routes.MultiVerb (AsUnion (..), GenericAsUnion (..), Respond, RespondEmpty, ResponseType, eitherFromUnion, eitherToUnion)
import Wire.API.Util.Aeson (CustomEncoded (CustomEncoded))

-- | These are just the "error" outcomes of the 'RemoveFromConversationResponses' type.
-- This is needed in using ExceptT to differentiate error outcomes from an
-- outcome reflecting a change.
data RemoveFromConversationError
  = RemoveFromConversationErrorRemovalNotAllowed
  | RemoveFromConversationErrorManagedConvNotAllowed
  | RemoveFromConversationErrorNotFound
  | RemoveFromConversationErrorCustomRolesNotSupported
  | RemoveFromConversationErrorSelfConv
  | RemoveFromConversationErrorOne2OneConv
  | RemoveFromConversationErrorConnectConv
  | RemoveFromConversationErrorUnchanged
  deriving stock (Eq, Show, Generic)
  deriving
    (ToJSON, FromJSON)
    via (CustomEncoded RemoveFromConversationError)
  deriving
    (AsUnion RemovalNotPerformedHTTPResponses)
    via (GenericAsUnion RemovalNotPerformedHTTPResponses RemoveFromConversationError)

instance GSOP.Generic RemoveFromConversationError

type RemovalNotPerformedHTTPResponses =
  '[ ConvMemberRemovalDenied,
     ManagedRemovalNotAllowed,
     ConvNotFound,
     CustomRolesNotSupported,
     InvalidOpSelfConv,
     InvalidOpOne2OneConv,
     InvalidOpConnectConv,
     RespondEmpty 204 "No change"
   ]

type RemoveFromConversationHTTPResponse =
  RemovalNotPerformedHTTPResponses
    .++ '[Respond 200 "Member removed" Public.Event]

type RemoveFromConversationResponse = Either RemoveFromConversationError Public.Event

instance
  ( rs ~ (RemovalNotPerformedHTTPResponses .++ '[r]),
    Public.Event ~ ResponseType r
  ) =>
  AsUnion rs RemoveFromConversationResponse
  where
  toUnion =
    eitherToUnion
      (toUnion @RemovalNotPerformedHTTPResponses)
      (Z . I)

  fromUnion =
    eitherFromUnion
      (fromUnion @RemovalNotPerformedHTTPResponses)
      (unI . unZ)
