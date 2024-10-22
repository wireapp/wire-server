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
{-# LANGUAGE DeriveAnyClass #-}

module Wire.API.Routes.Public.Galley.Messaging where

import Data.Id
import Data.OpenApi qualified as S
import Data.SOP
import Generics.SOP qualified as GSOP
import Imports
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Brig qualified as BrigError
import Wire.API.Error.Galley
import Wire.API.Message
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.QualifiedCapture
import Wire.API.ServantProto

type MessagingAPI =
  Named
    "post-otr-message-unqualified"
    ( Summary "Post an encrypted message to a conversation (accepts JSON or Protobuf)"
        :> Description PostOtrDescriptionUnqualified
        :> ZLocalUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "otr"
        :> "messages"
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> ReqBody '[JSON, Proto] NewOtrMessage
        :> MultiVerb
             'POST
             '[Servant.JSON]
             (PostOtrResponses ClientMismatch)
             (PostOtrResponse ClientMismatch)
    )
    :<|> Named
           "post-otr-broadcast-unqualified"
           ( Summary "Broadcast an encrypted message to all team members and all contacts (accepts JSON or Protobuf)"
               :> Description PostOtrDescriptionUnqualified
               :> ZLocalUser
               :> ZConn
               :> CanThrow 'TeamNotFound
               :> CanThrow 'BroadcastLimitExceeded
               :> CanThrow 'NonBindingTeam
               :> "broadcast"
               :> "otr"
               :> "messages"
               :> QueryParam "ignore_missing" IgnoreMissing
               :> QueryParam "report_missing" ReportMissing
               :> ReqBody '[JSON, Proto] NewOtrMessage
               :> MultiVerb
                    'POST
                    '[JSON]
                    (PostOtrResponses ClientMismatch)
                    (PostOtrResponse ClientMismatch)
           )
    :<|> Named
           "post-proteus-message"
           ( Summary "Post an encrypted message to a conversation (accepts only Protobuf)"
               :> Description PostOtrDescription
               :> ZLocalUser
               :> ZConn
               :> "conversations"
               :> QualifiedCapture "cnv" ConvId
               :> "proteus"
               :> "messages"
               :> ReqBody '[Proto] (RawProto QualifiedNewOtrMessage)
               :> MultiVerb
                    'POST
                    '[Servant.JSON]
                    (PostOtrResponses MessageSendingStatus)
                    (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
           )
    :<|> Named
           "post-proteus-broadcast"
           ( Summary "Post an encrypted message to all team members and all contacts (accepts only Protobuf)"
               :> Description PostOtrDescription
               :> ZLocalUser
               :> ZConn
               :> CanThrow 'TeamNotFound
               :> CanThrow 'BroadcastLimitExceeded
               :> CanThrow 'NonBindingTeam
               :> "broadcast"
               :> "proteus"
               :> "messages"
               :> ReqBody '[Proto] QualifiedNewOtrMessage
               :> MultiVerb
                    'POST
                    '[JSON]
                    (PostOtrResponses MessageSendingStatus)
                    (Either (MessageNotSent MessageSendingStatus) MessageSendingStatus)
           )

data MessageNotSent a
  = MessageNotSentConversationNotFound
  | MessageNotSentUnknownClient
  | MessageNotSentLegalholdOldClients
  | MessageNotSentLegalhold
  | MessageNotSentClientMissing a
  deriving stock (Eq, Show, Generic, Functor)
  deriving
    (AsUnion (MessageNotSentResponses a))
    via (GenericAsUnion (MessageNotSentResponses a) (MessageNotSent a))
  deriving anyclass (GSOP.Generic)

instance (S.ToSchema a) => S.ToSchema (MessageNotSent a)

type MessageNotSentResponses a =
  '[ ErrorResponse 'ConvNotFound,
     ErrorResponse 'BrigError.UnknownClient,
     ErrorResponse 'BrigError.MissingLegalholdConsentOldClients,
     ErrorResponse 'BrigError.MissingLegalholdConsent,
     Respond 412 "Missing clients" a
   ]

type PostOtrResponses a =
  MessageNotSentResponses a
    .++ '[Respond 201 "Message sent" a]

type PostOtrResponse a = Either (MessageNotSent a) a

instance
  ( rs ~ (MessageNotSentResponses a .++ '[r]),
    a ~ ResponseType r
  ) =>
  AsUnion rs (PostOtrResponse a)
  where
  toUnion =
    eitherToUnion
      (toUnion @(MessageNotSentResponses a))
      (Z . I)

  fromUnion =
    eitherFromUnion
      (fromUnion @(MessageNotSentResponses a))
      (unI . unZ)

-- This is a work-around for the fact that we sometimes want to send larger lists of user ids
-- in the filter query than fits the url length limit.  For details, see
-- https://github.com/zinfra/backend-issues/issues/1248
type PostOtrDescriptionUnqualified =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts two query params:\n\
  \- `ignore_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are ignored.\n\
  \  - When 'false' all missing clients are reported.\n\
  \  - When comma separated list of user-ids, only clients for listed users are ignored.\n\
  \- `report_missing`: Can be 'true' 'false' or a comma separated list of user IDs.\n\
  \  - When 'true' all missing clients are reported.\n\
  \  - When 'false' all missing clients are ignored.\n\
  \  - When comma separated list of user-ids, only clients for listed users are reported.\n\
  \\n\
  \Apart from these, the request body also accepts `report_missing` which can only be a list of user ids and behaves the same way as the query parameter.\n\
  \\n\
  \All three of these should be considered mutually exclusive. The server however does not error if more than one is specified, it reads them in this order of precedence:\n\
  \- `report_missing` in the request body has highest precedence.\n\
  \- `ignore_missing` in the query param is the next.\n\
  \- `report_missing` in the query param has the lowest precedence.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."

type PostOtrDescription =
  "This endpoint ensures that the list of clients is correct and only sends the message if the list is correct.\n\
  \To override this, the endpoint accepts `client_mismatch_strategy` in the body. It can have these values:\n\
  \- `report_all`: When set, the message is not sent if any clients are missing. The missing clients are reported in the response.\n\
  \- `ignore_all`: When set, no checks about missing clients are carried out.\n\
  \- `report_only`: Takes a list of qualified UserIDs. If any clients of the listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \- `ignore_only`: Takes a list of qualified UserIDs. If any clients of the non-listed users are missing, the message is not sent. The missing clients are reported in the response.\n\
  \\n\
  \The sending of messages in a federated conversation could theoretically fail partially. \
  \To make this case unlikely, the backend first gets a list of clients from all the involved backends and then tries to send a message. \
  \So, if any backend is down, the message is not propagated to anyone. \
  \But the actual message fan out to multiple backends could still fail partially. This type of failure is reported as a 201, \
  \the clients for which the message sending failed are part of the response body.\n\
  \\n\
  \This endpoint can lead to OtrMessageAdd event being sent to the recipients.\n\
  \\n\
  \**NOTE:** The protobuf definitions of the request body can be found at https://github.com/wireapp/generic-message-proto/blob/master/proto/otr.proto."
