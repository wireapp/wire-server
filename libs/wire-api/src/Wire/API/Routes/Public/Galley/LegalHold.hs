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

module Wire.API.Routes.Public.Galley.LegalHold where

import Data.Id
import GHC.Generics
import Generics.SOP qualified as GSOP
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MakesFederatedCall
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Team.LegalHold

type LegalHoldAPI =
  Named
    "create-legal-hold-settings"
    ( Summary "Create legal hold service settings"
        :> CanThrow 'NotATeamMember
        :> CanThrow OperationDenied
        :> CanThrow 'LegalHoldNotEnabled
        :> CanThrow 'LegalHoldServiceInvalidKey
        :> CanThrow 'LegalHoldServiceBadResponse
        :> ZLocalUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "legalhold"
        :> "settings"
        :> ReqBody '[JSON] NewLegalHoldService
        :> MultiVerb1 'POST '[JSON] (Respond 201 "Legal hold service settings created" ViewLegalHoldService)
    )
    :<|> Named
           "get-legal-hold-settings"
           ( Summary "Get legal hold service settings"
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> "settings"
               :> Get '[JSON] ViewLegalHoldService
           )
    :<|> Named
           "delete-legal-hold-settings"
           ( Summary "Delete legal hold service settings"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> CanThrow AuthenticationError
               :> CanThrow OperationDenied
               :> CanThrow 'NotATeamMember
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'InvalidOperation
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'LegalHoldDisableUnimplemented
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> CanThrow 'LegalHoldServiceBadResponse
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- ClientRemoved event to members with a legalhold client (via brig)\n\
                    \- UserLegalHoldDisabled event to contacts of members with a legalhold client (via brig)"
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> "settings"
               :> ReqBody '[JSON] RemoveLegalHoldSettingsRequest
               :> MultiVerb1 'DELETE '[JSON] (RespondEmpty 204 "Legal hold service settings deleted")
           )
    :<|> Named
           "get-legal-hold"
           ( Summary "Get legal hold status"
               :> CanThrow 'TeamMemberNotFound
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> Get '[JSON] UserLegalHoldStatusResponse
           )
    :<|> Named
           "consent-to-legal-hold"
           ( Summary "Consent to legal hold"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'InvalidOperation
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> "consent"
               :> MultiVerb 'POST '[JSON] GrantConsentResultResponseTypes GrantConsentResult
           )
    :<|> Named
           "request-legal-hold-device"
           ( Summary "Request legal hold device"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'TeamMemberNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'UserLegalHoldAlreadyEnabled
               :> CanThrow 'NoUserLegalHoldConsent
               :> CanThrow 'LegalHoldServiceBadResponse
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> CanThrow 'MLSLegalholdIncompatible
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- LegalHoldClientRequested event to contacts of the user the device is requested for, if they didn't already have a legalhold client (via brig)"
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> MultiVerb
                    'POST
                    '[JSON]
                    RequestDeviceResultResponseType
                    RequestDeviceResult
           )
    :<|> Named
           "disable-legal-hold-for-user"
           ( Summary "Disable legal hold for user"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> CanThrow AuthenticationError
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'NotATeamMember
               :> CanThrow OperationDenied
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> CanThrow 'LegalHoldServiceBadResponse
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- ClientRemoved event to the user owning the client (via brig)\n\
                    \- UserLegalHoldDisabled event to contacts of the user owning the client (via brig)"
               :> ZLocalUser
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> ReqBody '[JSON] DisableLegalHoldForUserRequest
               :> MultiVerb
                    'DELETE
                    '[JSON]
                    DisableLegalHoldForUserResponseType
                    DisableLegalHoldForUserResponse
           )
    :<|> Named
           "approve-legal-hold-device"
           ( Summary "Approve legal hold device"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> CanThrow AuthenticationError
               :> CanThrow 'AccessDenied
               :> CanThrow ('ActionDenied 'RemoveConversationMember)
               :> CanThrow 'NotATeamMember
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'UserLegalHoldNotPending
               :> CanThrow 'NoLegalHoldDeviceAllocated
               :> CanThrow 'LegalHoldServiceNotRegistered
               :> CanThrow 'UserLegalHoldAlreadyEnabled
               :> CanThrow 'UserLegalHoldIllegalOperation
               :> CanThrow 'LegalHoldCouldNotBlockConnections
               :> CanThrow 'LegalHoldServiceBadResponse
               :> Description
                    "This endpoint can lead to the following events being sent:\n\
                    \- ClientAdded event to the user owning the client (via brig)\n\
                    \- UserLegalHoldEnabled event to contacts of the user owning the client (via brig)\n\
                    \- ClientRemoved event to the user, if removing old client due to max number (via brig)"
               :> ZLocalUser
               :> ZConn
               :> "teams"
               :> Capture "tid" TeamId
               :> "legalhold"
               :> Capture "uid" UserId
               :> "approve"
               :> ReqBody '[JSON] ApproveLegalHoldForUserRequest
               :> MultiVerb1 'PUT '[JSON] (RespondEmpty 200 "Legal hold approved")
           )

type RequestDeviceResultResponseType =
  '[ RespondEmpty 201 "Request device successful",
     RespondEmpty 204 "Request device already pending"
   ]

data RequestDeviceResult
  = RequestDeviceSuccess
  | RequestDeviceAlreadyPending
  deriving (Generic)
  deriving (AsUnion RequestDeviceResultResponseType) via GenericAsUnion RequestDeviceResultResponseType RequestDeviceResult

instance GSOP.Generic RequestDeviceResult

type DisableLegalHoldForUserResponseType =
  '[ RespondEmpty 200 "Disable legal hold successful",
     RespondEmpty 204 "Legal hold was not enabled"
   ]

data DisableLegalHoldForUserResponse
  = DisableLegalHoldSuccess
  | DisableLegalHoldWasNotEnabled
  deriving (Generic)
  deriving (AsUnion DisableLegalHoldForUserResponseType) via GenericAsUnion DisableLegalHoldForUserResponseType DisableLegalHoldForUserResponse

instance GSOP.Generic DisableLegalHoldForUserResponse

type GrantConsentResultResponseTypes =
  '[ RespondEmpty 201 "Grant consent successful",
     RespondEmpty 204 "Consent already granted"
   ]

data GrantConsentResult
  = GrantConsentSuccess
  | GrantConsentAlreadyGranted
  deriving (Generic)
  deriving (AsUnion GrantConsentResultResponseTypes) via GenericAsUnion GrantConsentResultResponseTypes GrantConsentResult

instance GSOP.Generic GrantConsentResult
