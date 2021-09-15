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

import Control.Monad.Catch
import Data.Domain (Domain)
import Data.Id (UserId, idToText)
import Data.Qualified
import Galley.API.Util (viewFederationDomain)
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Types (convId)
import Galley.Types.Conversations.Members
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (+++))
import Wire.API.Conversation
import Wire.API.Federation.API.Galley

-- | View for a given user of a stored conversation.
--
-- Throws "bad-state" when the user is not part of the conversation.
conversationView :: UserId -> Data.Conversation -> Galley Conversation
conversationView uid conv = do
  localDomain <- viewFederationDomain
  let mbConv = conversationViewMaybe localDomain uid conv
  maybe memberNotFound pure mbConv
  where
    memberNotFound = do
      Log.err . msg $
        val "User "
          +++ idToText uid
          +++ val " is not a member of conv "
          +++ idToText (convId conv)
      throwM badState
    badState = mkError status500 "bad-state" "Bad internal member state."

-- | View for a given user of a stored conversation.
--
-- Returns 'Nothing' if the user is not part of the conversation.
conversationViewMaybe :: Domain -> UserId -> Data.Conversation -> Maybe Conversation
conversationViewMaybe localDomain uid conv = do
  let (selfs, lothers) = partition ((uid ==) . lmId) (Data.convLocalMembers conv)
      rothers = Data.convRemoteMembers conv
  self <- localMemberToSelf <$> listToMaybe selfs
  let others =
        map (localMemberToOther localDomain) lothers
          <> map remoteMemberToOther rothers
  pure $
    Conversation
      (Data.convMetadata localDomain conv)
      (ConvMembers self others)

-- | View for a local user of a remote conversation.
--
-- If the local user is not actually present in the conversation, simply
-- discard the conversation altogether. This should only happen if the remote
-- backend is misbehaving.
remoteConversationView ::
  UserId ->
  MemberStatus ->
  RemoteConversation ->
  Maybe Conversation
remoteConversationView uid status rconv = do
  let mems = rcnvMembers rconv
      others = rcmOthers mems
      self =
        localMemberToSelf
          LocalMember
            { lmId = uid,
              lmService = Nothing,
              lmStatus = status,
              lmConvRoleName = rcmSelfRole mems
            }
  pure $ Conversation (rcnvMetadata rconv) (ConvMembers self others)

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
      { rcnvMetadata = Data.convMetadata localDomain conv,
        rcnvMembers =
          RemoteConvMembers
            { rcmSelfRole = selfRole,
              rcmOthers = others
            }
      }

-- | Convert a local conversation member (as stored in the DB) to a publicly
-- facing 'Member' structure.
localMemberToSelf :: LocalMember -> Member
localMemberToSelf lm =
  Member
    { memId = lmId lm,
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
