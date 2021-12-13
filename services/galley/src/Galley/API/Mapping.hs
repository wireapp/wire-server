{-# LANGUAGE RecordWildCards #-}

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

module Galley.API.Mapping
  ( conversationView,
    conversationViewMaybe,
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
import Wire.API.Conversation hiding (Member (..))
import qualified Wire.API.Conversation as Conversation
import Wire.API.Federation.API.Galley

-- | View for a given user of a stored conversation.
--
-- Throws "bad-state" when the user is not part of the conversation.
conversationView ::
  Members '[Error InternalError, P.TinyLog] r =>
  Local UserId ->
  Data.Conversation ->
  Sem r Conversation
conversationView luid conv = do
  let mbConv = conversationViewMaybe luid conv
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
conversationViewMaybe :: Local UserId -> Data.Conversation -> Maybe Conversation
conversationViewMaybe luid conv = do
  let (selfs, lothers) = partition ((tUnqualified luid ==) . lmId) (Data.convLocalMembers conv)
      rothers = Data.convRemoteMembers conv
  self <- localMemberToSelf luid <$> listToMaybe selfs
  let others =
        map (localMemberToOther (tDomain luid)) lothers
          <> map remoteMemberToOther rothers
  pure $
    Conversation
      (qUntagged . qualifyAs luid . convId $ conv)
      (Data.convMetadata conv)
      (ConvMembers self others)

-- | View for a local user of a remote conversation.
remoteConversationView ::
  Local UserId ->
  MemberStatus ->
  Remote RemoteConversation ->
  Conversation
remoteConversationView uid status (qUntagged -> Qualified rconv rDomain) =
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
   in Conversation (Qualified (rcnvId rconv) rDomain) (rcnvMetadata rconv) (ConvMembers self others)

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
            }
      }

-- | Convert a local conversation member (as stored in the DB) to a publicly
-- facing 'Member' structure.
localMemberToSelf :: Local x -> LocalMember -> Conversation.Member
localMemberToSelf loc lm =
  Conversation.Member
    { memId = qUntagged . qualifyAs loc . lmId $ lm,
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
