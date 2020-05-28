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
import Data.ByteString.Conversion
import Data.Id
import qualified Data.List as List
import Galley.App
import qualified Galley.Data as Data
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import qualified System.Logger.Class as Log
import System.Logger.Message ((+++), msg, val)
import qualified Wire.API.Conversation as Public

conversationView :: UserId -> Data.Conversation -> Galley Public.Conversation
conversationView u Data.Conversation {..} = do
  let mm = toList convMembers
  let (me, them) = List.partition ((u ==) . Public.memId) mm
  m <- maybe memberNotFound return (listToMaybe me)
  let (name, mems) = (convName, Public.ConvMembers m (map toOther them))
  return $! Public.Conversation convId convType convCreator convAccess convAccessRole name mems convTeam convMessageTimer convReceiptMode
  where
    toOther x =
      Public.OtherMember
        { omId = Public.memId x,
          omService = Public.memService x,
          omConvRoleName = Public.memConvRoleName x
        }
    memberNotFound = do
      Log.err . msg $
        val "User "
          +++ toByteString u
          +++ val " is not a member of conv "
          +++ toByteString convId
      throwM badState
    badState = Error status500 "bad-state" "Bad internal member state."
