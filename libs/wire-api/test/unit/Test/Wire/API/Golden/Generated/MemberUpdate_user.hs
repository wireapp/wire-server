-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.MemberUpdate_user where

import Imports (Bool (False, True), Maybe (Just, Nothing), ($), (.))
import Wire.API.Conversation (MemberUpdate (..))
import Wire.API.Conversation.Member (MutedStatus (MutedStatus))

testObject_MemberUpdate_user_1 :: MemberUpdate
testObject_MemberUpdate_user_1 =
  MemberUpdate
    { mupOtrMuteStatus = Just . MutedStatus $ 0,
      mupOtrMuteRef = Just "h\52974N",
      mupOtrArchive = Just True,
      mupOtrArchiveRef = Just "ref",
      mupHidden = Just False,
      mupHiddenRef = Just ""
    }

testObject_MemberUpdate_user_2 :: MemberUpdate
testObject_MemberUpdate_user_2 =
  MemberUpdate
    { mupOtrMuteStatus = Nothing,
      mupOtrMuteRef = Nothing,
      mupOtrArchive = Nothing,
      mupOtrArchiveRef = Nothing,
      mupHidden = Just False,
      mupHiddenRef = Nothing
    }
