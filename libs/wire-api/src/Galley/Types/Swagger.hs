{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Types.Swagger where

import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified Wire.Swagger as Swagger

-- TODO(wire-api): check if all models are used
galleyModels :: [Doc.Model]
galleyModels =
  [ modelServiceRef,
    modelNewOtrMessage,
    modelOtrRecipients,
    modelOtrClientMap,
    modelClientMismatch,
    modelUserClients,
    modelUserIdList
  ]

typePriority :: Doc.DataType
typePriority =
  Doc.string $
    Doc.enum
      [ "low",
        "high"
      ]

modelNewOtrMessage :: Doc.Model
modelNewOtrMessage = Doc.defineModel "NewOtrMessage" $ do
  Doc.description "OTR message per recipient"
  Doc.property "sender" Doc.bytes' $
    Doc.description "The sender's client ID"
  Doc.property "recipients" (Doc.ref modelOtrRecipients) $
    Doc.description "Per-recipient data (i.e. ciphertext)."
  Doc.property "native_push" Doc.bool' $ do
    Doc.description "Whether to issue a native push to offline clients."
    Doc.optional
  Doc.property "transient" Doc.bool' $ do
    Doc.description "Whether to put this message into the notification queue."
    Doc.optional
  Doc.property "native_priority" typePriority $ do
    Doc.description "The native push priority (default 'high')."
    Doc.optional
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext) that is replicated \
      \for each recipient."
    Doc.optional
  Doc.property "report_missing" (Doc.unique $ Doc.array Doc.bytes') $ do
    Doc.description "List of user IDs"
    Doc.optional

modelOtrRecipients :: Doc.Model
modelOtrRecipients = Doc.defineModel "OtrRecipients" $ do
  Doc.description "Recipients of OTR content."
  Doc.property "" (Doc.ref modelOtrClientMap) $
    Doc.description "Mapping of user IDs to 'OtrClientMap's."

modelOtrClientMap :: Doc.Model
modelOtrClientMap = Doc.defineModel "OtrClientMap" $ do
  Doc.description "Map of client IDs to OTR content."
  Doc.property "" Doc.bytes' $
    Doc.description "Mapping from client IDs to OTR content (Base64 in JSON)."

modelClientMismatch :: Doc.Model
modelClientMismatch = Doc.defineModel "ClientMismatch" $ do
  Doc.description "Map of missing, redundant or deleted clients."
  Doc.property "time" Doc.dateTime' $
    Doc.description "Server timestamp (date and time)"
  Doc.property "missing" (Doc.ref modelUserClients) $
    Doc.description "Map of missing clients per user."
  Doc.property "redundant" (Doc.ref modelUserClients) $
    Doc.description "Map of redundant clients per user."
  Doc.property "deleted" (Doc.ref modelUserClients) $
    Doc.description "Map of deleted clients per user."

modelUserClients :: Doc.Model
modelUserClients =
  Doc.defineModel "UserClients"
    $ Doc.property "" (Doc.unique $ Doc.array Doc.bytes')
    $ Doc.description "Map of user IDs to sets of client IDs ({ UserId: [ClientId] })."

modelUserIdList :: Doc.Model
modelUserIdList = Doc.defineModel "UserIdList" $ do
  Doc.description "list of user ids"
  Doc.property "user_ids" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "the array of team conversations"

-- FUTUREWORK: unused? OtherMemberUpdateData doesn't exist.
modelOtherMemberUpdateData :: Doc.Model
modelOtherMemberUpdateData = Doc.defineModel "OtherMemberUpdateData" $ do
  Doc.description "Event data on other member updates"
  Doc.property "target" Doc.bytes' $ do
    Doc.description "Target ID of the user that the action was performed on"
    Doc.optional
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role to update to"
    Doc.optional

modelServiceRef :: Doc.Model
modelServiceRef = Doc.defineModel "ServiceRef" $ do
  Doc.description "Service Reference"
  Doc.property "id" Doc.bytes' $
    Doc.description "Service ID"
  Doc.property "provider" Doc.bytes' $
    Doc.description "Provider ID"

modelErrorObj :: Doc.Model
modelErrorObj = Swagger.errorModel
