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

module Wire.API.Routes.Public.Galley.MLS where

import Servant hiding (WithStatus)
import Servant.Swagger.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Servant
import Wire.API.MLS.Welcome
import Wire.API.MakesFederatedCall
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type MLSMessagingAPI =
  Named
    "mls-welcome-message"
    ( Summary "Post an MLS welcome message"
        :> Until 'V3
        :> MakesFederatedCall 'Galley "mls-welcome"
        :> CanThrow 'MLSKeyPackageRefNotFound
        :> CanThrow 'MLSNotEnabled
        :> "welcome"
        :> ZLocalUser
        :> ZConn
        :> ReqBody '[MLS] (RawMLS Welcome)
        :> MultiVerb1 'POST '[JSON] (RespondEmpty 201 "Welcome message sent")
    )
    :<|> Named
           "mls-message-v1"
           ( Summary "Post an MLS message"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Galley "send-mls-message"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-new-remote-conversation"
               :> MakesFederatedCall 'Brig "get-mls-clients"
               :> Until 'V2
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MLSMissingSenderClient
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow MLSProposalFailure
               :> "messages"
               :> ZLocalUser
               :> ZOptClient
               :> ZConn
               :> ReqBody '[MLS] (RawMLS SomeMessage)
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Message sent" [Event])
           )
    :<|> Named
           "mls-message"
           ( Summary "Post an MLS message"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Galley "send-mls-message"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-new-remote-conversation"
               :> MakesFederatedCall 'Brig "get-mls-clients"
               :> From 'V2
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MLSMissingSenderClient
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow MLSProposalFailure
               :> "messages"
               :> ZLocalUser
               :> ZOptClient
               :> ZConn
               :> ReqBody '[MLS] (RawMLS SomeMessage)
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Message sent" MLSMessageSendingStatus)
           )
    :<|> Named
           "mls-commit-bundle"
           ( Summary "Post a MLS CommitBundle"
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Galley "mls-welcome"
               :> MakesFederatedCall 'Galley "send-mls-commit-bundle"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Galley "on-new-remote-conversation"
               :> MakesFederatedCall 'Brig "get-mls-clients"
               :> From 'V4
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MLSMissingSenderClient
               :> CanThrow 'MLSWelcomeMismatch
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow MLSProposalFailure
               :> "commit-bundles"
               :> ZLocalUser
               :> ZOptClient
               :> ZConn
               :> ReqBody '[CommitBundleMimeType] CommitBundle
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Commit accepted and forwarded" MLSMessageSendingStatus)
           )
    :<|> Named
           "mls-public-keys"
           ( Summary "Get public keys used by the backend to sign external proposals"
               :> From 'V4
               :> CanThrow 'MLSNotEnabled
               :> "public-keys"
               :> ZLocalUser
               :> MultiVerb1 'GET '[JSON] (Respond 200 "Public keys" MLSPublicKeys)
           )

type MLSAPI = LiftNamed ("mls" :> MLSMessagingAPI)
