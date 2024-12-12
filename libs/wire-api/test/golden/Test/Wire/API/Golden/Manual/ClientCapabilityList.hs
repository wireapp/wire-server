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

module Test.Wire.API.Golden.Manual.ClientCapabilityList where

import Data.Set qualified as Set
import Imports
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.User.Client (ClientCapability (..), ClientCapabilityList (..))

testObject_ClientCapabilityList_1 :: Versioned V6 ClientCapabilityList
testObject_ClientCapabilityList_1 = Versioned $ ClientCapabilityList mempty

testObject_ClientCapabilityList_2 :: Versioned V6 ClientCapabilityList
testObject_ClientCapabilityList_2 = Versioned $ ClientCapabilityList (Set.fromList [ClientSupportsLegalholdImplicitConsent])

testObject_ClientCapabilityList_3 :: Versioned V6 ClientCapabilityList
testObject_ClientCapabilityList_3 =
  Versioned $
    ClientCapabilityList
      ( Set.fromList
          [ ClientSupportsLegalholdImplicitConsent,
            ClientSupportsConsumableNotifications
          ]
      )

testObject_ClientCapabilityList_3_V7 :: Versioned V7 ClientCapabilityList
testObject_ClientCapabilityList_3_V7 =
  Versioned $
    ClientCapabilityList
      ( Set.fromList
          [ ClientSupportsLegalholdImplicitConsent,
            ClientSupportsConsumableNotifications
          ]
      )

testObject_ClientCapabilityList_4 :: ClientCapabilityList
testObject_ClientCapabilityList_4 =
  ClientCapabilityList mempty

testObject_ClientCapabilityList_5 :: ClientCapabilityList
testObject_ClientCapabilityList_5 =
  ClientCapabilityList
    ( Set.fromList
        [ ClientSupportsLegalholdImplicitConsent,
          ClientSupportsConsumableNotifications
        ]
    )
