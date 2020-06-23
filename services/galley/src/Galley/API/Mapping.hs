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
import Data.Id (idToText)
import qualified Data.Id as Id
import Data.IdMapping (IdMapping (IdMapping, _imMappedId, _imQualifiedId), MappedOrLocalId (Local, Mapped), opaqueIdFromMappedOrLocal)
import qualified Data.List as List
import Data.Qualified (renderQualifiedId)
import Galley.App
import qualified Galley.Data as Data
import qualified Galley.Types.Conversations.Members as Internal
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (+++))
import qualified Wire.API.Conversation as Public

conversationView :: MappedOrLocalId Id.U -> Data.Conversation -> Galley Public.Conversation
conversationView u Data.Conversation {..} = do
  let mm = toList convMembers
  let (me, them) = List.partition ((u ==) . Internal.memId) mm
  m <- maybe memberNotFound return (listToMaybe me)
  let mems = Public.ConvMembers (toMember m) (toOther <$> them)
  return $! Public.Conversation convId convType convCreator convAccess convAccessRole convName mems convTeam convMessageTimer convReceiptMode
  where
    toOther :: Internal.Member -> Public.OtherMember
    toOther x =
      Public.OtherMember
        { Public.omId = opaqueIdFromMappedOrLocal (Internal.memId x),
          Public.omService = Internal.memService x,
          Public.omConvRoleName = Internal.memConvRoleName x
        }
    memberNotFound = do
      Log.err . msg $
        val "User "
          +++ showUserId u
          +++ val " is not a member of conv "
          +++ idToText convId
      throwM badState
    showUserId = \case
      Local localId ->
        idToText localId <> " (local)"
      Mapped IdMapping {_imMappedId, _imQualifiedId} ->
        idToText _imMappedId <> " (" <> renderQualifiedId _imQualifiedId <> ")"
    badState = Error status500 "bad-state" "Bad internal member state."

toMember :: Internal.Member -> Public.Member
toMember x@(Internal.Member {..}) =
  Public.Member {memId = opaqueIdFromMappedOrLocal (Internal.memId x), ..}
