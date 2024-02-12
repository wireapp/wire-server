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

module Test.Wire.API.Golden.Manual where

import Imports
import Test.Tasty
import Test.Wire.API.Golden.Manual.ClientCapability
import Test.Wire.API.Golden.Manual.ClientCapabilityList
import Test.Wire.API.Golden.Manual.Contact
import Test.Wire.API.Golden.Manual.ConvIdsPage
import Test.Wire.API.Golden.Manual.ConversationCoverView
import Test.Wire.API.Golden.Manual.ConversationEvent
import Test.Wire.API.Golden.Manual.ConversationPagingState
import Test.Wire.API.Golden.Manual.ConversationRemoveMembers
import Test.Wire.API.Golden.Manual.ConversationsResponse
import Test.Wire.API.Golden.Manual.CreateGroupConversation
import Test.Wire.API.Golden.Manual.CreateScimToken
import Test.Wire.API.Golden.Manual.FeatureConfigEvent
import Test.Wire.API.Golden.Manual.FederationDomainConfig
import Test.Wire.API.Golden.Manual.FederationRestriction
import Test.Wire.API.Golden.Manual.FederationStatus
import Test.Wire.API.Golden.Manual.GetPaginatedConversationIds
import Test.Wire.API.Golden.Manual.GroupId
import Test.Wire.API.Golden.Manual.ListConversations
import Test.Wire.API.Golden.Manual.ListUsersById
import Test.Wire.API.Golden.Manual.QualifiedUserClientPrekeyMap
import Test.Wire.API.Golden.Manual.SearchResultContact
import Test.Wire.API.Golden.Manual.SubConversation
import Test.Wire.API.Golden.Manual.TeamSize
import Test.Wire.API.Golden.Manual.Token
import Test.Wire.API.Golden.Manual.UserClientPrekeyMap
import Test.Wire.API.Golden.Manual.UserIdList
import Test.Wire.API.Golden.Runner
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned

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
      testGroup "ConversationEvent" $
        testObjects
          [ (testObject_Event_conversation_manual_1, "testObject_Event_conversation_manual_1.json"),
            (testObject_Event_conversation_manual_2, "testObject_Event_conversation_manual_2.json")
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
      testGroup "ConvIdsPage" $
        testObjects
          [ (testObject_ConvIdsPage_1, "testObject_ConvIdsPage_1.json"),
            (testObject_ConvIdsPage_2, "testObject_ConvIdsPage_2.json")
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
      testGroup "ConversationsResponse V2" $
        testObjects [(Versioned @'V2 testObject_ConversationsResponse_1, "testObject_ConversationsResponse_v2_1.json")],
      testGroup "ConversationsResponse" $
        testObjects [(testObject_ConversationsResponse_1, "testObject_ConversationsResponse_1.json")],
      testGroup "CreateScimToken" $
        testObjects
          [ (testObject_CreateScimToken_1, "testObject_CreateScimToken_1.json"),
            (testObject_CreateScimToken_2, "testObject_CreateScimToken_2.json"),
            (testObject_CreateScimToken_3, "testObject_CreateScimToken_3.json"),
            (testObject_CreateScimToken_4, "testObject_CreateScimToken_4.json")
          ],
      testGroup "Contact" $
        testObjects
          [ (testObject_Contact_1, "testObject_Contact_1.json"),
            (testObject_Contact_2, "testObject_Contact_2.json")
          ],
      testGroup "SearchResult Contact" $
        testObjects
          [ (testObject_SearchResultContact_1, "testObject_SearchResultContact_1.json"),
            (testObject_SearchResultContact_2, "testObject_SearchResultContact_2.json")
          ],
      testGroup "GroupId" $
        testObjects
          [(testObject_GroupId_1, "testObject_GroupId_1.json")],
      testGroup "PushToken" $
        testObjects
          [(testObject_Token_1, "testObject_Token_1.json")],
      testGroup "TeamSize" $
        testObjects
          [ (testObject_TeamSize_1, "testObject_TeamSize_1.json"),
            (testObject_TeamSize_2, "testObject_TeamSize_2.json"),
            (testObject_TeamSize_3, "testObject_TeamSize_3.json")
          ],
      testGroup "PublicSubConversation" $
        testObjects
          [ (testObject_PublicSubConversation_1, "testObject_PublicSubConversation_1.json"),
            (testObject_PublicSubConversation_2, "testObject_PublicSubConversation_2.json")
          ],
      testGroup "ListUsersById" $
        testObjects
          [ (testObject_ListUsersById_user_1, "testObject_ListUsersById_user_1.json"),
            (testObject_ListUsersById_user_2, "testObject_ListUsersById_user_2.json"),
            (testObject_ListUsersById_user_3, "testObject_ListUsersById_user_3.json")
          ],
      testGroup "CreateGroupConversation" $
        testObjects
          [ (testObject_CreateGroupConversation_1, "testObject_CreateGroupConversation_1.json"),
            (testObject_CreateGroupConversation_2, "testObject_CreateGroupConversation_2.json"),
            (testObject_CreateGroupConversation_3, "testObject_CreateGroupConversation_3.json")
          ],
      testGroup "FederationStatus" $
        testObjects
          [ (testObject_FederationStatus_1, "testObject_FederationStatus_1.json"),
            (testObject_FederationStatus_2, "testObject_FederationStatus_2.json")
          ],
      testGroup "FederationStatus.RemoteDomains" $
        testObjects
          [ (testObject_RemoteDomains_1, "testObject_RemoteDomains_1.json"),
            (testObject_RemoteDomains_2, "testObject_RemoteDomains_2.json")
          ],
      testGroup "FederationDomainConfig" $
        testObjects
          [ (testObject_FederationDomainConfig_1, "testObject_FederationDomainConfig_1.json"),
            (testObject_FederationDomainConfig_2, "testObject_FederationDomainConfig_2.json"),
            (testObject_FederationDomainConfig_3, "testObject_FederationDomainConfig_3.json")
          ],
      testGroup "FederationRestriction" $
        testObjects
          [ (testObject_FederationRestriction_1, "testObject_FederationRestriction_1.json"),
            (testObject_FederationRestriction_2, "testObject_FederationRestriction_2.json"),
            (testObject_FederationRestriction_3, "testObject_FederationRestriction_3.json")
          ],
      testGroup "ConversationRemoveMembers" $
        testObjects
          [ (testObject_ConversationRemoveMembers_1, "testObject_ConversationRemoveMembers_1.json"),
            (testObject_ConversationRemoveMembers_2, "testObject_ConversationRemoveMembers_2.json"),
            (testObject_ConversationRemoveMembers_3, "testObject_ConversationRemoveMembers_3.json")
          ]
    ]
