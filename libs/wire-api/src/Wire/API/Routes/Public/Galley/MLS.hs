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
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type MLSMessagingAPI =
  Named
    "mls-welcome-message"
    ( Summary "Post an MLS welcome message"
        :> CanThrow 'MLSKeyPackageRefNotFound
        :> "welcome"
        :> ZConn
        :> ReqBody '[MLS] (RawMLS Welcome)
        :> MultiVerb1 'POST '[JSON] (RespondEmpty 201 "Welcome message sent")
    )
    :<|> Named
           "mls-message-v1"
           ( Summary "Post an MLS message"
               :> Until 'V2
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow MLSProposalFailure
               :> "messages"
               :> ZOptClient
               :> ZConn
               :> ReqBody '[MLS] (RawMLS SomeMessage)
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Message sent" [Event])
           )
    :<|> Named
           "mls-message"
           ( Summary "Post an MLS message"
               :> From 'V2
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow MLSProposalFailure
               :> "messages"
               :> ZOptClient
               :> ZConn
               :> ReqBody '[MLS] (RawMLS SomeMessage)
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Message sent" MLSMessageSendingStatus)
           )
    :<|> Named
           "mls-commit-bundle"
           ( Summary "Post a MLS CommitBundle"
               :> From 'V3
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSKeyPackageRefNotFound
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MLSWelcomeMismatch
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow MLSProposalFailure
               :> "commit-bundles"
               :> ZOptClient
               :> ZConn
               :> ReqBody '[CommitBundleMimeType] CommitBundle
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Commit accepted and forwarded" MLSMessageSendingStatus)
           )
    :<|> Named
           "mls-public-keys"
           ( Summary "Get public keys used by the backend to sign external proposals"
               :> "public-keys"
               :> MultiVerb1 'GET '[JSON] (Respond 200 "Public keys" MLSPublicKeys)
           )

type MLSAPI = LiftNamed (ZLocalUser :> "mls" :> MLSMessagingAPI)
