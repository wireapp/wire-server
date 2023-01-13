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

module Galley.API.Mapping
  ( conversationView,
    conversationViewWithCachedOthers,
    remoteConversationView,
    conversationToRemote,
    localMemberToSelf,
  )
where

import Data.Domain (Domain)
import Data.Id (UserId, idToText)
import Data.Qualified
import Galley.API.Error
import qualified Galley.Data.Conversation as Data
import Galley.Data.Types (convId)
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Polysemy.Error
import qualified Polysemy.TinyLog as P
import System.Logger.Message (msg, val, (+++))
import Wire.API.Conversation
import qualified Wire.API.Conversation as Conversation
import Wire.API.Federation.API.Galley

-- | View for a given user of a stored conversation.
--
-- Throws @BadMemberState@ when the user is not part of the conversation.
conversationView ::
  Members '[Error InternalError, P.TinyLog] r =>
  Local UserId ->
  Data.Conversation ->
  Sem r Conversation
conversationView luid conv = do
  let remoteOthers = map remoteMemberToOther $ Data.convRemoteMembers conv
      localOthers = map (localMemberToOther (tDomain luid)) $ Data.convLocalMembers conv
  conversationViewWithCachedOthers remoteOthers localOthers conv luid

-- | Like 'conversationView' but optimized for situations which could benefit
-- from pre-computing the list of @OtherMember@s in the conversation. For
-- instance, creating @ConvesationView@ for more than 1 member of the same conversation.
conversationViewWithCachedOthers ::
  Members '[Error InternalError, P.TinyLog] r =>
  [OtherMember] ->
  [OtherMember] ->
  Data.Conversation ->
  Local UserId ->
  Sem r Conversation
conversationViewWithCachedOthers remoteOthers localOthers conv luid = do
  let mbConv = conversationViewMaybe luid remoteOthers localOthers conv
  maybe memberNotFound pure mbConv
  where
    memberNotFound = do
      P.err . msg $
        val "User "
          +++ idToText (tUnqualified luid)
          +++ val " is not a member of conv "
          +++ idToText (convId conv)
      throw BadMemberState

-- | View for a given user of a stored conversation.
--
-- Returns 'Nothing' if the user is not part of the conversation.
conversationViewMaybe :: Local UserId -> [OtherMember] -> [OtherMember] -> Data.Conversation -> Maybe Conversation
conversationViewMaybe luid remoteOthers localOthers conv = do
  let selfs = filter ((tUnqualified luid ==) . lmId) (Data.convLocalMembers conv)
  self <- localMemberToSelf luid <$> listToMaybe selfs
  let others = filter (\oth -> tUntagged luid /= omQualifiedId oth) localOthers <> remoteOthers
  pure $
    Conversation
      (tUntagged . qualifyAs luid . convId $ conv)
      (Data.convMetadata conv)
      (ConvMembers self others)
      (Data.convProtocol conv)

-- | View for a local user of a remote conversation.
remoteConversationView ::
  Local UserId ->
  MemberStatus ->
  Remote RemoteConversation ->
  Conversation
remoteConversationView uid status (tUntagged -> Qualified rconv rDomain) =
  let mems = rcnvMembers rconv
      others = rcmOthers mems
      self =
        localMemberToSelf
          uid
          LocalMember
            { lmId = tUnqualified uid,
              lmService = Nothing,
              lmStatus = status,
              lmConvRoleName = rcmSelfRole mems
            }
   in Conversation
        (Qualified (rcnvId rconv) rDomain)
        (rcnvMetadata rconv)
        (ConvMembers self others)
        (rcnvProtocol rconv)

-- | Convert a local conversation to a structure to be returned to a remote
-- backend.
--
-- This returns 'Nothing' if the given remote user is not part of the conversation.
conversationToRemote ::
  Domain ->
  Remote UserId ->
  Data.Conversation ->
  Maybe RemoteConversation
conversationToRemote localDomain ruid conv = do
  let (selfs, rothers) = partition ((== ruid) . rmId) (Data.convRemoteMembers conv)
      lothers = Data.convLocalMembers conv
  selfRole <- rmConvRoleName <$> listToMaybe selfs
  let others =
        map (localMemberToOther localDomain) lothers
          <> map remoteMemberToOther rothers
  pure $
    RemoteConversation
      { rcnvId = Data.convId conv,
        rcnvMetadata = Data.convMetadata conv,
        rcnvMembers =
          RemoteConvMembers
            { rcmSelfRole = selfRole,
              rcmOthers = others
            },
        rcnvProtocol = Data.convProtocol conv
      }

-- | Convert a local conversation member (as stored in the DB) to a publicly
-- facing 'Member' structure.
localMemberToSelf :: Local x -> LocalMember -> Conversation.Member
localMemberToSelf loc lm =
  Conversation.Member
    { memId = tUntagged . qualifyAs loc . lmId $ lm,
      memService = lmService lm,
      memOtrMutedStatus = msOtrMutedStatus st,
      memOtrMutedRef = msOtrMutedRef st,
      memOtrArchived = msOtrArchived st,
      memOtrArchivedRef = msOtrArchivedRef st,
      memHidden = msHidden st,
      memHiddenRef = msHiddenRef st,
      memConvRoleName = lmConvRoleName lm
    }
  where
    st = lmStatus lm
