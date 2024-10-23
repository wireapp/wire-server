-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.Push
  ( testObject_Push_1,
    testObject_Push_2,
  )
where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Id
import Data.List1
import Data.Range
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Imports
import Wire.API.Push.V2

rcp1, rcp2, rcp3 :: Recipient
rcp1 =
  Recipient
    (Id . fromJust $ UUID.fromString "15441ff8-7f14-11ef-aeec-bbe21dc8a204")
    RouteAny
    RecipientClientsAll
rcp2 =
  Recipient
    (Id . fromJust $ UUID.fromString "2e18540e-7f14-11ef-9886-d3c2ff21d3d1")
    RouteDirect
    (RecipientClientsSome (list1 (ClientId 0) []))
rcp3 =
  Recipient
    (Id . fromJust $ UUID.fromString "316924ee-7f14-11ef-b6a2-036a4f646914")
    RouteDirect
    (RecipientClientsSome (list1 (ClientId 234) [ClientId 123]))

testObject_Push_1 :: Push
testObject_Push_1 =
  Push
    { _pushRecipients = unsafeRange (Set.fromList [rcp1]),
      _pushOrigin = Nothing,
      _pushConnections = mempty,
      _pushOriginConnection = Nothing,
      _pushTransient = False,
      _pushNativeIncludeOrigin = False,
      _pushNativeEncrypt = True,
      _pushNativeAps = Nothing,
      _pushNativePriority = HighPriority,
      _pushPayload = singleton mempty
    }

testObject_Push_2 :: Push
testObject_Push_2 =
  Push
    { _pushRecipients = unsafeRange (Set.fromList [rcp2, rcp3]),
      _pushOrigin = Just (Id . fromJust $ UUID.fromString "dec9b47a-7f12-11ef-b634-6710e7ae3d33"),
      _pushConnections = Set.fromList [ConnId "sdf", ConnId "mempty", ConnId "wire-client"],
      _pushOriginConnection = Just (ConnId "123"),
      _pushTransient = True,
      _pushNativeIncludeOrigin = True,
      _pushNativeEncrypt = False,
      _pushNativeAps = Just (apsData (ApsLocKey "asdf") ["1", "22", "333"]),
      _pushNativePriority = LowPriority,
      _pushPayload =
        list1
          (KM.fromList [("foo" :: KM.Key) A..= '3', "bar" A..= True])
          [KM.fromList [], KM.fromList ["growl" A..= ("foooood" :: Text)], KM.fromList ["lunchtime" A..= ("imminent" :: Text)]]
    }
