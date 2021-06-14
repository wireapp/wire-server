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
import Data.Tagged (unTagged)
import Galley.API.Util (viewFederationDomain)
import Galley.App
import qualified Galley.Data as Data
import Galley.Data.Types (convId)
import qualified Galley.Types.Conversations.Members as Internal
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
  let localMembers = localToOther localDomain <$> convLocalMembers
  let remoteMembers = remoteToOther <$> convRemoteMembers
  let (me, otherMembers) = List.partition ((qUid ==) . Public.omQualifiedId) (localMembers <> remoteMembers)
  let userAndConvOnSameBackend = find ((qUnqualified qUid ==) . Internal.memId) convLocalMembers
  let selfMember =
        -- if the user and the conversation are on the same backend, we can create a real self member
        -- otherwise, we need to fall back to a default self member (see futurework)
        -- (Note: the extra domain check is done to catch the edge case where two users in a conversation have the same unqualified UUID)
        if isJust userAndConvOnSameBackend && localDomain == qDomain qUid
          then toMember <$> userAndConvOnSameBackend
          else incompleteSelfMember me
  selfMember <&> \m -> do
    let mems = Public.ConvMembers m otherMembers
    Public.Conversation convId convType convCreator convAccess convAccessRole convName mems convTeam convMessageTimer convReceiptMode
  where
    localToOther :: Domain -> Internal.LocalMember -> Public.OtherMember
    localToOther domain x =
      Public.OtherMember
        { Public.omQualifiedId = Qualified (Internal.memId x) domain,
          Public.omService = Internal.memService x,
          Public.omConvRoleName = Internal.memConvRoleName x
        }

    remoteToOther :: Internal.RemoteMember -> Public.OtherMember
    remoteToOther x =
      Public.OtherMember
        { Public.omQualifiedId = unTagged (Internal.rmId x),
          Public.omService = Nothing,
          Public.omConvRoleName = Internal.rmConvRoleName x
        }

    -- FUTUREWORK(federation): we currently don't store muted, archived etc status for users who are on a different backend than a conversation
    -- but we should. Once this information is available, the code should be changed to use the stored information, rather than these defaults.
    incompleteSelfMember :: [Public.OtherMember] -> Maybe Public.Member
    incompleteSelfMember [] = Nothing
    incompleteSelfMember [m] =
      Just $
        Public.Member
          (qUnqualified (Public.omQualifiedId m))
          Nothing
          False
          Nothing
          Nothing
          False
          Nothing
          False
          Nothing
          (Public.omConvRoleName m)
    incompleteSelfMember _ = Nothing -- FUTUREWORK: throw an error here, shouldn't happen

toMember :: Internal.LocalMember -> Public.Member
toMember x@Internal.InternalMember {..} =
  Public.Member {memId = Internal.memId x, ..}
