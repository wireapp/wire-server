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

module Galley.API.MLS.SubConversation where

import Data.Id
import Data.Qualified
import Galley.API.Error
import Galley.API.MLS
import Galley.API.MLS.GroupInfo
import Galley.API.MLS.Types
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.App (Env)
import qualified Galley.Data.Conversation as Data
import Galley.Data.Conversation.Types
import Galley.Effects
import Galley.Effects.SubConversationStore
import qualified Galley.Effects.SubConversationStore as Eff
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import qualified Polysemy.TinyLog as P
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Error (FederationError, federationNotImplemented)
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.SubConversation

getSubConversation ::
  Members
    '[ SubConversationStore,
       ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvAccessDenied,
       ErrorS 'MLSSubConvUnsupportedConvType,
       Error InternalError,
       Error Wai.Error,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  Qualified ConvId ->
  SubConvId ->
  Sem r PublicSubConversation
getSubConversation lusr qconv sconv = do
  foldQualified
    lusr
    (\lcnv -> getLocalSubConversation lusr lcnv sconv)
    (\_rcnv -> throw federationNotImplemented)
    qconv

getLocalSubConversation ::
  Members
    '[ SubConversationStore,
       ConversationStore,
       ErrorS 'ConvNotFound,
       ErrorS 'ConvAccessDenied,
       ErrorS 'MLSSubConvUnsupportedConvType,
       Error InternalError,
       P.TinyLog
     ]
    r =>
  Local UserId ->
  Local ConvId ->
  SubConvId ->
  Sem r PublicSubConversation
getLocalSubConversation lusr lconv sconv = do
  c <- getConversationAndCheckMembership (tUntagged lusr) lconv

  unless (Data.convType c == RegularConv) $
    throwS @'MLSSubConvUnsupportedConvType

  msub <- Eff.getSubConversation (tUnqualified lconv) sconv
  sub <- case msub of
    Nothing -> do
      mlsMeta <- noteS @'ConvNotFound (mlsMetadata c)
      -- deriving this detemernistically to prevent race condition between
      -- multiple threads creating the subconversation
      let groupId = initialGroupId lconv sconv
          epoch = Epoch 0
          suite = cnvmlsCipherSuite mlsMeta
      createSubConversation (tUnqualified lconv) sconv suite epoch groupId Nothing
      setGroupIdForSubConversation groupId (tUntagged lconv) sconv
      let sub =
            SubConversation
              { scParentConvId = tUnqualified lconv,
                scSubConvId = sconv,
                scMLSData =
                  ConversationMLSData
                    { cnvmlsGroupId = groupId,
                      cnvmlsEpoch = epoch,
                      cnvmlsCipherSuite = suite
                    },
                scMembers = mkClientMap []
              }
      pure sub
    Just sub -> pure sub
  pure (toPublicSubConv (tUntagged (qualifyAs lusr sub)))

getSubConversationGroupInfo ::
  Members
    '[ ConversationStore,
       Error FederationError,
       FederatorAccess,
       Input Env,
       MemberStore,
       SubConversationStore
     ]
    r =>
  Members MLSGroupInfoStaticErrors r =>
  Local UserId ->
  Qualified ConvId ->
  SubConvId ->
  Sem r OpaquePublicGroupState
getSubConversationGroupInfo lusr qcnvId subconv = do
  assertMLSEnabled
  foldQualified
    lusr
    (getSubConversationGroupInfoFromLocalConv (tUntagged lusr) subconv)
    (getGroupInfoFromRemoteConv lusr . fmap (flip SubConv subconv))
    qcnvId

getSubConversationGroupInfoFromLocalConv ::
  Members
    '[ ConversationStore,
       SubConversationStore,
       MemberStore
     ]
    r =>
  Members MLSGroupInfoStaticErrors r =>
  Qualified UserId ->
  SubConvId ->
  Local ConvId ->
  Sem r OpaquePublicGroupState
getSubConversationGroupInfoFromLocalConv qusr subConvId lcnvId = do
  void $ getLocalConvForUser qusr lcnvId
  getSubConversationPublicGroupState (tUnqualified lcnvId) subConvId
    >>= noteS @'MLSMissingGroupInfo
