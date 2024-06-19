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

module Galley.API.MLS.GroupInfo where

import Data.Id as Id
import Data.Json.Util
import Data.Qualified
import Galley.API.MLS.Enabled
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.Effects
import Galley.Effects.ConversationStore qualified as E
import Galley.Effects.FederatorAccess qualified as E
import Galley.Env
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.SubConversation

type MLSGroupInfoStaticErrors =
  '[ ErrorS 'ConvNotFound,
     ErrorS 'MLSMissingGroupInfo,
     ErrorS 'MLSNotEnabled
   ]

getGroupInfo ::
  ( Member ConversationStore r,
    Member (Error FederationError) r,
    Member FederatorAccess r,
    Member (Input Env) r,
    Member MemberStore r
  ) =>
  (Members MLSGroupInfoStaticErrors r) =>
  Local UserId ->
  Qualified ConvId ->
  Sem r GroupInfoData
getGroupInfo lusr qcnvId = do
  assertMLSEnabled
  foldQualified
    lusr
    (getGroupInfoFromLocalConv . tUntagged $ lusr)
    (getGroupInfoFromRemoteConv lusr . fmap Conv)
    qcnvId

getGroupInfoFromLocalConv ::
  ( Member ConversationStore r,
    Member MemberStore r
  ) =>
  (Members MLSGroupInfoStaticErrors r) =>
  Qualified UserId ->
  Local ConvId ->
  Sem r GroupInfoData
getGroupInfoFromLocalConv qusr lcnvId = do
  void $ getLocalConvForUser qusr lcnvId
  E.getGroupInfo (tUnqualified lcnvId)
    >>= noteS @'MLSMissingGroupInfo

getGroupInfoFromRemoteConv ::
  ( Member (Error FederationError) r,
    Member FederatorAccess r
  ) =>
  (Members MLSGroupInfoStaticErrors r) =>
  Local UserId ->
  Remote ConvOrSubConvId ->
  Sem r GroupInfoData
getGroupInfoFromRemoteConv lusr rcnv = do
  let getRequest =
        GetGroupInfoRequest
          { sender = tUnqualified lusr,
            conv = tUnqualified rcnv
          }
  response <- E.runFederated rcnv (fedClient @'Galley @"query-group-info" getRequest)
  case response of
    GetGroupInfoResponseError e -> rethrowErrors @MLSGroupInfoStaticErrors e
    GetGroupInfoResponseState s ->
      pure
        . GroupInfoData
        . fromBase64ByteString
        $ s
