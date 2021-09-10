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

module Galley.API.Mapping where

import Control.Monad.Catch
import Data.Domain (Domain)
import Data.Id (UserId, idToText)
import qualified Data.List as List
import Data.Qualified (Qualified (..))
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
import qualified Wire.API.Conversation as Public

-- | View for a given user of a stored conversation.
-- Throws "bad-state" when the user is not part of the conversation.
conversationView :: UserId -> Data.Conversation -> Galley Public.Conversation
conversationView uid conv = do
  mbConv <- conversationViewMaybe uid conv
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

conversationViewMaybe :: UserId -> Data.Conversation -> Galley (Maybe Public.Conversation)
conversationViewMaybe u conv = do
  localDomain <- viewFederationDomain
  pure $ conversationViewMaybeQualified localDomain (Qualified u localDomain) conv

-- | View for a given user of a stored conversation.
-- Returns 'Nothing' when the user is not part of the conversation.
conversationViewMaybeQualified :: Domain -> Qualified UserId -> Data.Conversation -> Maybe Public.Conversation
conversationViewMaybeQualified localDomain qUid Data.Conversation {..} = do
  let localMembers = localMemberToOther localDomain <$> convLocalMembers
  let remoteMembers = remoteMemberToOther <$> convRemoteMembers
  let me = List.find ((qUid ==) . Public.omQualifiedId) (localMembers <> remoteMembers)
  let otherMembers = filter ((qUid /=) . Public.omQualifiedId) (localMembers <> remoteMembers)
  let userAndConvOnSameBackend = find ((qUnqualified qUid ==) . lmId) convLocalMembers
  let selfMember =
        -- if the user and the conversation are on the same backend, we can create a real self member
        -- otherwise, we need to fall back to a default self member (see futurework)
        -- (Note: the extra domain check is done to catch the edge case where two users in a conversation have the same unqualified UUID)
        if isJust userAndConvOnSameBackend && localDomain == qDomain qUid
          then localToSelf <$> userAndConvOnSameBackend
          else remoteToSelf <$> me
  selfMember <&> \m -> do
    let mems = Public.ConvMembers m otherMembers
    Public.mkConversation
      (Qualified convId localDomain)
      convType
      convCreator
      convAccess
      convAccessRole
      convName
      mems
      convTeam
      convMessageTimer
      convReceiptMode

-- FUTUREWORK(federation): we currently don't store muted, archived etc status for users who are on a different backend than a conversation
-- but we should. Once this information is available, the code should be changed to use the stored information, rather than these defaults.
remoteToSelf :: Public.OtherMember -> Public.Member
remoteToSelf m =
  Public.Member
    { memId = qUnqualified (Public.omQualifiedId m),
      memService = Nothing,
      memOtrMutedStatus = Nothing,
      memOtrMutedRef = Nothing,
      memOtrArchived = False,
      memOtrArchivedRef = Nothing,
      memHidden = False,
      memHiddenRef = Nothing,
      memConvRoleName = Public.omConvRoleName m
    }

localToSelf :: LocalMember -> Public.Member
localToSelf lm =
  Public.Member
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
