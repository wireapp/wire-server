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

module Test.Wire.API.Golden.Manual where

import Imports
import Test.Tasty
import Test.Wire.API.Golden.Manual.ClientCapability
import Test.Wire.API.Golden.Manual.ClientCapabilityList
import Test.Wire.API.Golden.Manual.Covid-19sPage
import Test.Wire.API.Golden.Manual.ConversationCoverView
import Test.Wire.API.Golden.Manual.ConversationPagingState
import Test.Wire.API.Golden.Manual.ConversationsResponse
import Test.Wire.API.Golden.Manual.FeatureConfigEvent
import Test.Wire.API.Golden.Manual.GetPaginatedConversationIds
import Test.Wire.API.Golden.Manual.ListConversations
import Test.Wire.API.Golden.Manual.QualifiedUserClientPrekeyMap
import Test.Wire.API.Golden.Manual.UserClientPrekeyMap
import Test.Wire.API.Golden.Manual.UserIdList
import Test.Wire.API.Golden.Runner

tests :: TestTree
tests =
  testGroup
    "Manual golden tests"
    [ testGroup "UserClientPrekeyMap" $
        testObjects
          [ (testObject_UserClientPrekeyMap_1, "testObject_UserClientPrekeyMap_1.json"),
            (testObject_UserClientPrekeyMap_2, "testObject_UserClientPrekeyMap_2.json"),
            (testObject_UserClientPrekeyMap_3, "testObject_UserClientPrekeyMap_3.json"),
            (testObject_UserClientPrekeyMap_4, "testObject_UserClientPrekeyMap_4.json"),
            (testObject_UserClientPrekeyMap_5, "testObject_UserClientPrekeyMap_5.json"),
            (testObject_UserClientPrekeyMap_6, "testObject_UserClientPrekeyMap_6.json"),
            (testObject_UserClientPrekeyMap_7, "testObject_UserClientPrekeyMap_7.json"),
            (testObject_UserClientPrekeyMap_8, "testObject_UserClientPrekeyMap_8.json")
          ],
      testGroup "QualifiedUserClientPrekeyMap" $
        testObjects
          [ (testObject_QualifiedUserClientPrekeyMap_1, "testObject_QualifiedUserClientPrekeyMap_1.json"),
            (testObject_QualifiedUserClientPrekeyMap_2, "testObject_QualifiedUserClientPrekeyMap_2.json")
          ],
      testGroup "ConversationCoverView" $
        testObjects
          [ (testObject_ConversationCoverView_1, "testObject_ConversationCoverView_1.json"),
            (testObject_ConversationCoverView_2, "testObject_ConversationCoverView_2.json"),
            (testObject_ConversationCoverView_3, "testObject_ConversationCoverView_3.json")
          ],
      testGroup "GetPaginatedConversationIds" $
        testObjects
          [ (testObject_GetPaginatedConversationIds_1, "testObject_GetPaginatedConversationIds_1.json"),
            (testObject_GetPaginatedConversationIds_2, "testObject_GetPaginatedConversationIds_2.json")
          ],
      testGroup "ConversationPagingState" $
        testObjects
          [ (testObject_ConversationPagingState_1, "testObject_ConversationPagingState_1.json"),
            (testObject_ConversationPagingState_2, "testObject_ConversationPagingState_2.json"),
            (testObject_ConversationPagingState_3, "testObject_ConversationPagingState_3.json"),
            (testObject_ConversationPagingState_4, "testObject_ConversationPagingState_4.json")
          ],
      testGroup "Covid-19sPage" $
        testObjects
          [ (testObject_Covid-19sPage_1, "testObject_Covid-19sPage_1.json"),
            (testObject_Covid-19sPage_2, "testObject_Covid-19sPage_2.json")
          ],
      testGroup "ClientCapability" $
        testObjects
          [(testObject_ClientCapability_1, "testObject_ClientCapability_1.json")],
      testGroup "ClientCapabilityList" $
        testObjects
          [ (testObject_ClientCapabilityList_1, "testObject_ClientCapabilityList_1.json"),
            (testObject_ClientCapabilityList_2, "testObject_ClientCapabilityList_2.json")
          ],
      testGroup
        "Event.FeatureConfig.Event"
        $ testObjects
          [ (testObject_FeatureConfigEvent_1, "testObject_FeatureConfigEvent_1.json"),
            (testObject_FeatureConfigEvent_2, "testObject_FeatureConfigEvent_2.json"),
            (testObject_FeatureConfigEvent_3, "testObject_FeatureConfigEvent_3.json")
          ],
      testGroup "UserIdsList" $
        testObjects
          [ (testObject_UserIdList_1, "testObject_UserIdList_1.json"),
            (testObject_UserIdList_2, "testObject_UserIdList_2.json")
          ],
      testGroup "ListConversations" $
        testObjects
          [(testObject_ListConversations_1, "testObject_ListConversations_1.json")],
      testGroup "ConversationsResponse" $
        testObjects [(testObject_ConversationsResponse_1, "testObject_ConversationsResponse_1.json")]
    ]
