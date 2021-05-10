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

import Data.Aeson
  ( Value (Array, Bool, Null, Number, Object, String),
  )
import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports
  ( Bool (False, True),
    Functor (fmap),
    Maybe (Just, Nothing),
    fromJust,
    read,
  )
import Wire.API.Notification
  ( QueuedNotificationList,
    queuedNotification,
    queuedNotificationList,
  )

testObject_QueuedNotificationList_user_1 :: QueuedNotificationList
testObject_QueuedNotificationList_user_1 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("", Null), ("p", Bool True)], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002")))) ((List1 (NonEmpty.fromList [fromList [("", Array [String "\SO"])], fromList []]))))]) (True) (fmap read (Just "1864-05-19 07:34:20.509238926493 UTC")))

testObject_QueuedNotificationList_user_2 :: QueuedNotificationList
testObject_QueuedNotificationList_user_2 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("", Array [Number (0.0), Null])], fromList [("", Array [String "", Null])]])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("", String "")]])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("\1002760", Array [])], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList []]))))]) (False) (fmap read (Just "1864-04-29 13:53:36.621523022731 UTC")))

testObject_QueuedNotificationList_user_3 :: QueuedNotificationList
testObject_QueuedNotificationList_user_3 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-16 13:53:35.281384700276 UTC")))

testObject_QueuedNotificationList_user_4 :: QueuedNotificationList
testObject_QueuedNotificationList_user_4 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("", Array [Null]), ("\7770c", Number (-20.0))], fromList [], fromList [], fromList [], fromList [], fromList []]))))]) (True) (fmap read (Just "1864-05-07 03:04:03.677917355524 UTC")))

testObject_QueuedNotificationList_user_5 :: QueuedNotificationList
testObject_QueuedNotificationList_user_5 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("y", Object (fromList [("", String "")]))], fromList [("!", Array [])]])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("", Object (fromList [(":", Null)]))], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList [], fromList []]))))]) (True) (fmap read (Just "1864-05-04 20:04:00.385969583352 UTC")))

testObject_QueuedNotificationList_user_6 :: QueuedNotificationList
testObject_QueuedNotificationList_user_6 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-10 02:08:41.125447101921 UTC")))

testObject_QueuedNotificationList_user_7 :: QueuedNotificationList
testObject_QueuedNotificationList_user_7 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000001")))) ((List1 (NonEmpty.fromList [fromList [("\45585I\171412\SUB", Array []), ("", Object (fromList [])), ("'\8002'V\174940", Null), ("r", String "\34871u\143514DH")], fromList [("", Object (fromList [("1", Null)]))], fromList [], fromList []]))))]) (False) (fmap read (Nothing)))

testObject_QueuedNotificationList_user_8 :: QueuedNotificationList
testObject_QueuedNotificationList_user_8 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000004")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList [], fromList []]))))]) (False) (fmap read (Just "1864-05-06 19:34:07.842851307868 UTC")))

testObject_QueuedNotificationList_user_9 :: QueuedNotificationList
testObject_QueuedNotificationList_user_9 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000003")))) ((List1 (NonEmpty.fromList [fromList [("^e[\1086653i", Array [Null]), ("l", Array [String "yv\bu\a"]), ("B8+#", String "\SUB`\1088953"), ("\NUL&k\1085174", Object (fromList [("e\EM\990200$", Number (4.0e-3))])), ("c\138868", Null)], fromList []]))))]) (True) (fmap read (Just "1864-05-19 05:14:44.883081744636 UTC")))

testObject_QueuedNotificationList_user_10 :: QueuedNotificationList
testObject_QueuedNotificationList_user_10 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("", Object (fromList []))]])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [("", Object (fromList [("", String "")]))]]))))]) (True) (fmap read (Just "1864-05-14 22:59:36.632886313527 UTC")))

testObject_QueuedNotificationList_user_11 :: QueuedNotificationList
testObject_QueuedNotificationList_user_11 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Nothing)))

testObject_QueuedNotificationList_user_12 :: QueuedNotificationList
testObject_QueuedNotificationList_user_12 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-04 09:15:29.205195175891 UTC")))

testObject_QueuedNotificationList_user_13 :: QueuedNotificationList
testObject_QueuedNotificationList_user_13 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-01 19:33:18.971740786102 UTC")))

testObject_QueuedNotificationList_user_14 :: QueuedNotificationList
testObject_QueuedNotificationList_user_14 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-04-30 07:58:44.856820480203 UTC")))

testObject_QueuedNotificationList_user_15 :: QueuedNotificationList
testObject_QueuedNotificationList_user_15 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-01 05:18:36.710631729776 UTC")))

testObject_QueuedNotificationList_user_16 :: QueuedNotificationList
testObject_QueuedNotificationList_user_16 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-12 20:30:51.221043991768 UTC")))

testObject_QueuedNotificationList_user_17 :: QueuedNotificationList
testObject_QueuedNotificationList_user_17 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Nothing)))

testObject_QueuedNotificationList_user_18 :: QueuedNotificationList
testObject_QueuedNotificationList_user_18 = (queuedNotificationList ([]) (True) (fmap read (Nothing)))

testObject_QueuedNotificationList_user_19 :: QueuedNotificationList
testObject_QueuedNotificationList_user_19 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [], fromList [], fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("\1062330", String "")], fromList []]))))]) (True) (fmap read (Just "1864-05-01 08:57:14.649508884242 UTC")))

testObject_QueuedNotificationList_user_20 :: QueuedNotificationList
testObject_QueuedNotificationList_user_20 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))), (queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-01 03:16:29.788087572971 UTC")))
