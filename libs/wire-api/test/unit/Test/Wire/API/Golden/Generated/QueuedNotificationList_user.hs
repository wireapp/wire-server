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
module Test.Wire.API.Golden.Generated.QueuedNotificationList_user where

import Data.Aeson (Value (Bool, Null))
import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Bool (False, True), Functor (fmap), Maybe (Just, Nothing), fromJust, read)
import Wire.API.Notification (QueuedNotificationList, queuedNotification, queuedNotificationList)

testObject_QueuedNotificationList_user_1 :: QueuedNotificationList
testObject_QueuedNotificationList_user_1 =
  queuedNotificationList
    [ queuedNotification
        (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000")))
        ( List1
            ( NonEmpty.fromList
                [ fromList [("", Null), ("p", Bool True)]
                ]
            )
        )
    ]
    True
    (fmap read (Just "1864-05-19 07:34:20.509238926493 UTC"))

testObject_QueuedNotificationList_user_2 :: QueuedNotificationList
testObject_QueuedNotificationList_user_2 =
  queuedNotificationList [] False Nothing
