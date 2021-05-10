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
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Galley.API.Mapping where

import Control.Monad.Catch
import Data.Id (UserId, idToText)
import qualified Data.List as List
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
    badState = Error status500 "bad-state" "Bad internal member state."

-- | View for a given user of a stored conversation.
-- Returns 'Nothing' when the user is not part of the conversation.
conversationViewMaybe :: UserId -> Data.Conversation -> Galley (Maybe Public.Conversation)
conversationViewMaybe u Data.Conversation {..} = do
  let mm = toList convMembers
  let (me, them) = List.partition ((u ==) . Internal.memId) mm
  for (listToMaybe me) $ \m -> do
    let mems = Public.ConvMembers (toMember m) (toOther <$> them)
    return $! Public.Conversation convId convType convCreator convAccess convAccessRole convName mems convTeam convMessageTimer convReceiptMode
  where
    toOther :: Internal.LocalMember -> Public.OtherMember
    toOther x =
      Public.OtherMember
        { Public.omId = Internal.memId x,
          Public.omService = Internal.memService x,
          Public.omConvRoleName = Internal.memConvRoleName x
        }

toMember :: Internal.LocalMember -> Public.Member
toMember x@Internal.Member {..} =
  Public.Member {memId = Internal.memId x, ..}
