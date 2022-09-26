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
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.Effects
import qualified Galley.Effects.ConversationStore as E
import qualified Galley.Effects.FederatorAccess as E
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.PublicGroupState

type MLSGroupInfoStaticErrors =
  '[ ErrorS 'ConvNotFound,
     ErrorS 'MLSMissingGroupInfo
   ]

getGroupInfo ::
  Members
    '[ ConversationStore,
       Error FederationError,
       FederatorAccess,
       Input (Local ()),
       MemberStore
     ]
    r =>
  Members MLSGroupInfoStaticErrors r =>
  Local UserId ->
  Qualified ConvId ->
  Sem r OpaquePublicGroupState
getGroupInfo lusr qcnvId =
  foldQualified
    lusr
    (getGroupInfoFromLocalConv . qUntagged $ lusr)
    (getGroupInfoFromRemoteConv lusr)
    qcnvId

getGroupInfoFromLocalConv ::
  Members
    '[ ConversationStore,
       MemberStore,
       Input (Local ())
     ]
    r =>
  Members MLSGroupInfoStaticErrors r =>
  Qualified UserId ->
  Local ConvId ->
  Sem r OpaquePublicGroupState
getGroupInfoFromLocalConv qusr lcnvId = do
  void $ getLocalConvForUser qusr lcnvId
  E.getPublicGroupState (tUnqualified lcnvId)
    >>= noteS @'MLSMissingGroupInfo

getGroupInfoFromRemoteConv ::
  Members '[Error FederationError, FederatorAccess] r =>
  Members MLSGroupInfoStaticErrors r =>
  Local UserId ->
  Remote ConvId ->
  Sem r OpaquePublicGroupState
getGroupInfoFromRemoteConv lusr rcnv = do
  let getRequest =
        GetGroupInfoRequest
          { ggireqSender = tUnqualified lusr,
            ggireqConv = tUnqualified rcnv
          }
  response <- E.runFederated rcnv (fedClient @'Galley @"query-group-info" getRequest)
  case response of
    GetGroupInfoResponseError e -> rethrowErrors @MLSGroupInfoStaticErrors e
    GetGroupInfoResponseState s ->
      pure . OpaquePublicGroupState
        . fromBase64ByteString
        $ s
