{-# LANGUAGE OverloadedLists #-}

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
module Test.Wire.API.Golden.Generated.ConversationAccessUpdate_user where

import Wire.API.Conversation
  ( Access (InviteAccess),
    AccessRole (ActivatedAccessRole, NonActivatedAccessRole),
    ConversationAccessUpdate (..),
  )

testObject_ConversationAccessUpdate_user_1 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_1 =
  ConversationAccessUpdate {cupAccess = [], cupAccessRole = NonActivatedAccessRole}

testObject_ConversationAccessUpdate_user_2 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_2 =
  ConversationAccessUpdate {cupAccess = [InviteAccess], cupAccessRole = ActivatedAccessRole}

testObject_ConversationAccessUpdate_user_3 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_3 =
  ConversationAccessUpdate {cupAccess = [], cupAccessRole = NonActivatedAccessRole}

testObject_ConversationAccessUpdate_user_4 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_4 =
  ConversationAccessUpdate {cupAccess = [LinkAccess, InviteAccess], cupAccessRole = TeamAccessRole}

testObject_ConversationAccessUpdate_user_5 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_5 =
  ConversationAccessUpdate
    { cupAccess = [CodeAccess, PrivateAccess, PrivateAccess, PrivateAccess, CodeAccess, CodeAccess, LinkAccess],
      cupAccessRole = NonActivatedAccessRole
    }

testObject_ConversationAccessUpdate_user_6 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_6 =
  ConversationAccessUpdate {cupAccess = [PrivateAccess, InviteAccess, InviteAccess], cupAccessRole = TeamAccessRole}

testObject_ConversationAccessUpdate_user_7 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_7 =
  ConversationAccessUpdate {cupAccess = [LinkAccess], cupAccessRole = TeamAccessRole}

testObject_ConversationAccessUpdate_user_8 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_8 =
  ConversationAccessUpdate {cupAccess = [LinkAccess, InviteAccess], cupAccessRole = ActivatedAccessRole}

testObject_ConversationAccessUpdate_user_9 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_9 =
  ConversationAccessUpdate
    { cupAccess =
        [ CodeAccess,
          LinkAccess,
          InviteAccess,
          PrivateAccess,
          LinkAccess,
          CodeAccess,
          PrivateAccess,
          InviteAccess,
          CodeAccess,
          InviteAccess,
          PrivateAccess,
          CodeAccess,
          LinkAccess
        ],
      cupAccessRole = ActivatedAccessRole
    }

testObject_ConversationAccessUpdate_user_10 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_10 =
  ConversationAccessUpdate
    { cupAccess = [LinkAccess, PrivateAccess, CodeAccess, InviteAccess],
      cupAccessRole = ActivatedAccessRole
    }

testObject_ConversationAccessUpdate_user_11 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_11 =
  ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = ActivatedAccessRole}

testObject_ConversationAccessUpdate_user_12 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_12 =
  ConversationAccessUpdate {cupAccess = [InviteAccess, PrivateAccess], cupAccessRole = ActivatedAccessRole}

testObject_ConversationAccessUpdate_user_13 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_13 =
  ConversationAccessUpdate {cupAccess = [InviteAccess, CodeAccess], cupAccessRole = TeamAccessRole}

testObject_ConversationAccessUpdate_user_14 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_14 =
  ConversationAccessUpdate
    { cupAccess = [LinkAccess, CodeAccess, InviteAccess, LinkAccess, CodeAccess, CodeAccess, CodeAccess],
      cupAccessRole = TeamAccessRole
    }

testObject_ConversationAccessUpdate_user_15 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_15 =
  ConversationAccessUpdate {cupAccess = [InviteAccess, CodeAccess, CodeAccess], cupAccessRole = ActivatedAccessRole}

testObject_ConversationAccessUpdate_user_16 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_16 =
  ConversationAccessUpdate
    { cupAccess =
        [InviteAccess, LinkAccess, PrivateAccess, PrivateAccess, LinkAccess, CodeAccess, CodeAccess, CodeAccess],
      cupAccessRole = ActivatedAccessRole
    }

testObject_ConversationAccessUpdate_user_17 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_17 =
  ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = PrivateAccessRole}

testObject_ConversationAccessUpdate_user_18 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_18 =
  ConversationAccessUpdate
    { cupAccess =
        [PrivateAccess, InviteAccess, PrivateAccess, PrivateAccess, LinkAccess, CodeAccess, PrivateAccess, InviteAccess],
      cupAccessRole = NonActivatedAccessRole
    }

testObject_ConversationAccessUpdate_user_19 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_19 =
  ConversationAccessUpdate {cupAccess = [LinkAccess, CodeAccess], cupAccessRole = PrivateAccessRole}

testObject_ConversationAccessUpdate_user_20 :: ConversationAccessUpdate
testObject_ConversationAccessUpdate_user_20 =
  ConversationAccessUpdate {cupAccess = [], cupAccessRole = NonActivatedAccessRole}
