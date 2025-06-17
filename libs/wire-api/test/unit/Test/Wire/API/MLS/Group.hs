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

module Test.Wire.API.MLS.Group where

import Data.Qualified
import Imports
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Conversation
import Wire.API.MLS.Group
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.SubConversation

tests :: TestTree
tests =
  testGroup
    "Group"
    [ testProperty "roundtrip serialise and parse groupId" $ roundtripGroupId
    ]

roundtripGroupId :: GroupIdVersion -> ConvType -> Qualified ConvOrSubConvId -> GroupIdGen -> Property
roundtripGroupId v ct convId gen =
  let gen' = case qUnqualified convId of
        Conv _ | v == GroupIdVersion1 -> GroupIdGen 0
        _ -> gen
   in groupIdToConv
        ( convToGroupId
            v
            GroupIdParts
              { convType = ct,
                qConvId = convId,
                gidGen = gen
              }
        )
        === Right
          ( Just v,
            GroupIdParts
              { convType = ct,
                qConvId = convId,
                gidGen = gen'
              }
          )
