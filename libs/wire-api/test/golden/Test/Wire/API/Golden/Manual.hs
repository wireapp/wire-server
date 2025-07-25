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
import Test.Tasty.HUnit
import Test.Wire.API.Golden.Manual.Activate_user
import Test.Wire.API.Golden.Manual.CannonId
import Test.Wire.API.Golden.Manual.ClientCapability
import Test.Wire.API.Golden.Manual.ClientCapabilityList
import Test.Wire.API.Golden.Manual.Contact
import Test.Wire.API.Golden.Manual.ConversationCoverView
import Test.Wire.API.Golden.Manual.ConversationEvent
import Test.Wire.API.Golden.Manual.ConversationPagingState
import Test.Wire.API.Golden.Manual.ConversationRemoveMembers
import Test.Wire.API.Golden.Manual.ConversationsResponse
import Test.Wire.API.Golden.Manual.CreateGroupConversation
import Test.Wire.API.Golden.Manual.CreateScimToken
import Test.Wire.API.Golden.Manual.CreateScimTokenResponse
import Test.Wire.API.Golden.Manual.DomainVerification
import Test.Wire.API.Golden.Manual.EnterpriseLogin
import Test.Wire.API.Golden.Manual.FeatureConfigEvent
import Test.Wire.API.Golden.Manual.FederationDomainConfig
import Test.Wire.API.Golden.Manual.FederationRestriction
import Test.Wire.API.Golden.Manual.FederationStatus
import Test.Wire.API.Golden.Manual.GetPaginatedConversationIds
import Test.Wire.API.Golden.Manual.GroupId
import Test.Wire.API.Golden.Manual.InvitationUserView
import Test.Wire.API.Golden.Manual.ListConversations
import Test.Wire.API.Golden.Manual.ListUsersById
import Test.Wire.API.Golden.Manual.LoginId_user
import Test.Wire.API.Golden.Manual.Login_user
import Test.Wire.API.Golden.Manual.MLSKeys
import Test.Wire.API.Golden.Manual.Presence
import Test.Wire.API.Golden.Manual.Push
import Test.Wire.API.Golden.Manual.PushRemove
import Test.Wire.API.Golden.Manual.QualifiedUserClientPrekeyMap
import Test.Wire.API.Golden.Manual.SearchResultContact
import Test.Wire.API.Golden.Manual.SendActivationCode_user
import Test.Wire.API.Golden.Manual.SubConversation
import Test.Wire.API.Golden.Manual.TeamSize
import Test.Wire.API.Golden.Manual.Token
import Test.Wire.API.Golden.Manual.UserClientPrekeyMap
import Test.Wire.API.Golden.Manual.UserEvent
import Test.Wire.API.Golden.Manual.UserGroup
import Test.Wire.API.Golden.Manual.UserIdList
import Test.Wire.API.Golden.Runner
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned

tests :: TestTree
tests =
  testGroup
    "Manual golden tests"
    [ testGroup "NewUserGroup" $
        testObjects
          [ (testObject_NewUserGroup_1, "testObject_NewUserGroup_1.json"),
            (testObject_NewUserGroup_2, "testObject_NewUserGroup_2.json")
          ],
      testGroup "NewUserGroup" $
        testObjects
          [ (testObject_UserGroup_1, "testObject_UserGroup_1.json"),
            (testObject_UserGroup_2, "testObject_UserGroup_2.json")
          ],
      testGroup "UserGrouppUpdate" $
        testObjects
          [ (testObject_UserGroupUpdate_1, "testObject_UserGroupUpdate_1.json"),
            (testObject_UserGroupUpdate_2, "testObject_UserGroupUpdate_2.json")
          ],
      testGroup "UserClientPrekeyMap" $
        testObjects
          [ (testObject_UserClientPrekeyMap_1, "testObject_UserClientPrekeyMap_1.json"),
            (testObject_UserClientPrekeyMap_2, "testObject_UserClientPrekeyMap_2.json"),
            (testObject_UserClientPrekeyMap_3, "testObject_UserClientPrekeyMap_3.json"),
            (testObject_UserClientPrekeyMap_4, "testObject_UserClientPrekeyMap_4.json"),
            (testObject_UserClientPrekeyMap_5, "testObject_UserClientPrekeyMap_5.json"),
            (testObject_UserClientPrekeyMap_6, "testObject_UserClientPrekeyMap_6.json"),
            (testObject_UserClientPrekeyMap_7, "testObject_UserClientPrekeyMap_7.json"),
            (testObject_UserClientPrekeyMap_8, "testObject_UserClientPrekeyMap_8.json"),
            (testObject_UserClientPrekeyMap_9, "testObject_UserClientPrekeyMap_9.json")
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
      testGroup "ClientCapability" $
        testObjects
          [ (testObject_ClientCapability_1, "testObject_ClientCapability_1.json"),
            (testObject_ClientCapability_2, "testObject_ClientCapability_2.json")
          ],
      testGroup "ClientCapabilityListV6" $
        testObjects
          [ (testObject_ClientCapabilityList_1, "testObject_ClientCapabilityList_1.json"),
            (testObject_ClientCapabilityList_2, "testObject_ClientCapabilityList_2.json")
          ],
      testGroup "ClientCapabilityListV6 - non-round-trip" $
        [ testToJSON testObject_ClientCapabilityList_3 "testObject_ClientCapabilityList_3.json",
          testToJSON testObject_ClientCapabilityList_3_V7 "testObject_ClientCapabilityList_3_V7.json"
        ],
      testGroup "ClientCapabilityList" $
        testObjects
          [ (testObject_ClientCapabilityList_4, "testObject_ClientCapabilityList_4.json"),
            (testObject_ClientCapabilityList_5, "testObject_ClientCapabilityList_5.json")
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
      testGroup "ConversationsResponse V5" $
        testObjects [(Versioned @'V5 testObject_ConversationsResponse_1, "testObject_ConversationsResponse_v5_1.json")],
      testGroup "ConversationsResponse" $
        testObjects [(testObject_ConversationsResponse_1, "testObject_ConversationsResponse_1.json")],
      testGroup "CreateScimToken" $
        testObjects
          [ (testObject_CreateScimToken_1, "testObject_CreateScimToken_1.json"),
            (testObject_CreateScimToken_2, "testObject_CreateScimToken_2.json"),
            (testObject_CreateScimToken_3, "testObject_CreateScimToken_3.json"),
            (testObject_CreateScimToken_4, "testObject_CreateScimToken_4.json")
          ],
      testGroup "CreateScimTokenResponse" $
        testObjects
          [ (testObject_CreateScimTokenResponse_1, "testObject_CreateScimTokenResponse_1.json")
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
      testGroup "PublicSubConversationV5" $
        testObjects
          [ ( Versioned @'V5 testObject_PublicSubConversation_1,
              "testObject_PublicSubConversation_v5_1.json"
            ),
            ( Versioned @'V5 testObject_PublicSubConversation_2,
              "testObject_PublicSubConversation_v5_2.json"
            )
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
          ],
      testGroup "UserEvent" $
        testObjects
          [ (testObject_UserEvent_1, "testObject_UserEvent_1.json"),
            (testObject_UserEvent_2, "testObject_UserEvent_2.json"),
            (testObject_UserEvent_3, "testObject_UserEvent_3.json"),
            (testObject_UserEvent_4, "testObject_UserEvent_4.json"),
            (testObject_UserEvent_5, "testObject_UserEvent_5.json"),
            (testObject_UserEvent_6, "testObject_UserEvent_6.json"),
            (testObject_UserEvent_7, "testObject_UserEvent_7.json"),
            (testObject_UserEvent_8, "testObject_UserEvent_8.json"),
            (testObject_UserEvent_9, "testObject_UserEvent_9.json"),
            (testObject_UserEvent_10, "testObject_UserEvent_10.json"),
            (testObject_UserEvent_11, "testObject_UserEvent_11.json"),
            (testObject_UserEvent_12, "testObject_UserEvent_12.json"),
            (testObject_UserEvent_13, "testObject_UserEvent_13.json"),
            (testObject_UserEvent_14, "testObject_UserEvent_14.json"),
            (testObject_UserEvent_15, "testObject_UserEvent_15.json"),
            (testObject_UserEvent_16, "testObject_UserEvent_16.json"),
            (testObject_UserEvent_17, "testObject_UserEvent_17.json"),
            (testObject_UserEvent_18, "testObject_UserEvent_18.json")
          ],
      testGroup "MLSPublicKeys" $
        testObjects
          [ (testObject_MLSPublicKeys1, "testObject_MLSPublicKeys_1.json")
          ],
      testGroup "MLSKeysByPurpose" $
        testObjects
          [ (testObject_MLSKeysByPurpose1, "testObject_MLSKeysByPurpose_1.json")
          ],
      testGroup "SendActivationCode" $
        testObjects
          [ (testObject_SendActivationCode_1, "testObject_SendActivationCode_1.json"),
            (testObject_SendActivationCode_2, "testObject_SendActivationCode_2.json")
          ],
      testGroup "LoginId" $
        testObjects
          [ (testObject_LoginId_user_1, "testObject_LoginId_user_1.json"),
            (testObject_LoginId_user_2, "testObject_LoginId_user_2.json"),
            (testObject_LoginId_user_3, "testObject_LoginId_user_3.json"),
            (testObject_LoginId_user_4, "testObject_LoginId_user_4.json"),
            (testObject_LoginId_user_5, "testObject_LoginId_user_5.json"),
            (testObject_LoginId_user_6, "testObject_LoginId_user_6.json")
          ],
      testGroup "Login" $
        testObjects
          [ (testObject_Login_user_1, "testObject_Login_user_1.json"),
            (testObject_Login_user_2, "testObject_Login_user_2.json"),
            (testObject_Login_user_3, "testObject_Login_user_3.json"),
            (testObject_Login_user_4, "testObject_Login_user_4.json"),
            (testObject_Login_user_5, "testObject_Login_user_5.json")
          ],
      testGroup "CannonId" $
        testObjects
          [ (testObject_CannonId_1, "testObject_CannonId_1.json"),
            (testObject_CannonId_2, "testObject_CannonId_2.json"),
            (testObject_CannonId_3, "testObject_CannonId_3.json")
          ],
      testGroup "Presence" $
        testObjects
          [ (testObject_Presence_1, "testObject_Presence_1.json"),
            (testObject_Presence_2, "testObject_Presence_2.json")
          ],
      testGroup "Push" $
        testObjects
          [ (testObject_Push_1, "testObject_Push_1.json"),
            (testObject_Push_2, "testObject_Push_2.json")
          ],
      testGroup "PushRemove" $
        testObjects
          [ (testObject_PushRemove_1, "testObject_PushRemove_1.json")
          ],
      testGroup "Activate" $
        testObjects
          [ (testObject_Activate_user_1, "testObject_Activate_user_1.json"),
            (testObject_Activate_user_2, "testObject_Activate_user_2.json"),
            (testObject_Activate_user_3, "testObject_Activate_user_3.json"),
            (testObject_Activate_user_4, "testObject_Activate_user_4.json")
          ],
      testGroup "InvitationUserView" $
        testObjects
          [ (testObject_InvitationUserView_team_1, "testObject_InvitationUserView_team_1.json"),
            (testObject_InvitationUserView_team_2, "testObject_InvitationUserView_team_2.json")
          ],
      testGroup "DomainRegistrationResponse" $
        [ testGroup "V9" $
            testObjects
              [ (testObject_DomainRegistrationResponseV9_1, "testObject_DomainRegistrationResponseV9_1.json"),
                (testObject_DomainRegistrationResponseV9_2, "testObject_DomainRegistrationResponseV9_2.json"),
                (testObject_DomainRegistrationResponseV9_3, "testObject_DomainRegistrationResponseV9_3.json"),
                (testObject_DomainRegistrationResponseV9_4, "testObject_DomainRegistrationResponseV9_4.json"),
                (testObject_DomainRegistrationResponseV9_5, "testObject_DomainRegistrationResponseV9_5.json"),
                (testObject_DomainRegistrationResponseV9_6, "testObject_DomainRegistrationResponseV9_6.json")
              ]
              ++ [ testCase
                     "non-isomorph in webappUrl"
                     (assertJSONIsGolden testObject_DomainRegistrationResponseV9_7 "testObject_DomainRegistrationResponseV9_7.json")
                 ],
          testGroup "V10" $
            testObjects
              [ (testObject_DomainRegistrationResponseV10_1, "testObject_DomainRegistrationResponseV10_1.json"),
                (testObject_DomainRegistrationResponseV10_2, "testObject_DomainRegistrationResponseV10_2.json"),
                (testObject_DomainRegistrationResponseV10_3, "testObject_DomainRegistrationResponseV10_3.json"),
                (testObject_DomainRegistrationResponseV10_4, "testObject_DomainRegistrationResponseV10_4.json"),
                (testObject_DomainRegistrationResponseV10_5, "testObject_DomainRegistrationResponseV10_5.json"),
                (testObject_DomainRegistrationResponseV10_6, "testObject_DomainRegistrationResponseV10_6.json")
              ]
              ++ [ testCase
                     "non-isomorph in webappUrl"
                     (assertJSONIsGolden testObject_DomainRegistrationResponseV10_7 "testObject_DomainRegistrationResponseV10_7.json")
                 ]
        ],
      testGroup "DomainRegistrationUpdate" $
        testObjects
          [ (testObject_DomainRegistrationUpdate_1, "testObject_DomainRegistrationUpdate_1.json"),
            (testObject_DomainRegistrationUpdate_2, "testObject_DomainRegistrationUpdate_2.json"),
            (testObject_DomainRegistrationUpdate_3, "testObject_DomainRegistrationUpdate_3.json"),
            (testObject_DomainRegistrationUpdate_4, "testObject_DomainRegistrationUpdate_4.json"),
            (testObject_DomainRegistrationUpdate_5, "testObject_DomainRegistrationUpdate_5.json"),
            (testObject_DomainRegistrationUpdate_6, "testObject_DomainRegistrationUpdate_6.json")
          ]
          ++ [ testCase
                 "non-isomorph in webappUrl"
                 (assertJSONIsGolden testObject_DomainRegistrationUpdate_7 "testObject_DomainRegistrationUpdate_7.json")
             ],
      testGroup
        "DomainRedirectResponse"
        $ [ testGroup "V9" $
              testObjects
                [ (testObject_DomainRedirectResponseV9_1, "testObject_DomainRedirectResponseV9_1.json"),
                  (testObject_DomainRedirectResponseV9_2, "testObject_DomainRedirectResponseV9_2.json"),
                  (testObject_DomainRedirectResponseV9_3, "testObject_DomainRedirectResponseV9_3.json"),
                  (testObject_DomainRedirectResponseV9_4, "testObject_DomainRedirectResponseV9_4.json"),
                  (testObject_DomainRedirectResponseV9_6, "testObject_DomainRedirectResponseV9_6.json"),
                  (testObject_DomainRedirectResponseV9_7, "testObject_DomainRedirectResponseV9_7.json")
                ]
                ++ [ testCase
                       "non-isomorph in webappUrl"
                       (assertJSONIsGolden testObject_DomainRedirectResponseV9_5 "testObject_DomainRedirectResponseV9_5.json")
                   ],
            testGroup "V10" $
              testObjects
                [ (testObject_DomainRedirectResponseV10_1, "testObject_DomainRedirectResponseV10_1.json"),
                  (testObject_DomainRedirectResponseV10_2, "testObject_DomainRedirectResponseV10_2.json"),
                  (testObject_DomainRedirectResponseV10_3, "testObject_DomainRedirectResponseV10_3.json"),
                  (testObject_DomainRedirectResponseV10_4, "testObject_DomainRedirectResponseV10_4.json"),
                  (testObject_DomainRedirectResponseV10_5, "testObject_DomainRedirectResponseV10_5.json"),
                  (testObject_DomainRedirectResponseV10_6, "testObject_DomainRedirectResponseV10_6.json"),
                  (testObject_DomainRedirectResponseV10_7, "testObject_DomainRedirectResponseV10_7.json")
                ]
          ],
      testGroup
        "DomainRedirectConfig"
        $ [ testGroup "V9" $
              testObjects
                [ (testObject_DomainRedirectConfigV9_1, "testObject_DomainRedirectConfigV9_1.json"),
                  (testObject_DomainRedirectConfigV9_3, "testObject_DomainRedirectConfigV9_3.json"),
                  (testObject_DomainRedirectConfigV9_4, "testObject_DomainRedirectConfigV9_4.json")
                ]
                ++ [ testCase
                       "non-isomorph in webappUrl"
                       (assertJSONIsGolden testObject_DomainRedirectConfigV9_2 "testObject_DomainRedirectConfigV9_2.json")
                   ],
            testGroup "V10" $
              testObjects
                [ (testObject_DomainRedirectConfig_1, "testObject_DomainRedirectConfig_1.json"),
                  (testObject_DomainRedirectConfig_2, "testObject_DomainRedirectConfig_2.json"),
                  (testObject_DomainRedirectConfig_4, "testObject_DomainRedirectConfig_4.json")
                ]
          ]
    ]
