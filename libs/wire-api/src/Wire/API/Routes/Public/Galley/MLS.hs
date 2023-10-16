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
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Servant
import Wire.API.MakesFederatedCall
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version

type MLSMessagingAPI =
  Named
    "mls-message"
    ( Summary "Post an MLS message"
        :> From 'V5
        :> MakesFederatedCall 'Galley "on-mls-message-sent"
        :> MakesFederatedCall 'Galley "send-mls-message"
        :> MakesFederatedCall 'Galley "on-conversation-updated"
        :> MakesFederatedCall 'Brig "get-mls-clients"
        :> CanThrow 'ConvAccessDenied
        :> CanThrow 'ConvMemberNotFound
        :> CanThrow 'ConvNotFound
        :> CanThrow 'LegalHoldNotEnabled
        :> CanThrow 'MissingLegalholdConsent
        :> CanThrow 'MLSClientMismatch
        :> CanThrow 'MLSClientSenderUserMismatch
        :> CanThrow 'MLSCommitMissingReferences
        :> CanThrow 'MLSGroupConversationMismatch
        :> CanThrow 'MLSInvalidLeafNodeIndex
        :> CanThrow 'MLSNotEnabled
        :> CanThrow 'MLSProposalNotFound
        :> CanThrow 'MLSProtocolErrorTag
        :> CanThrow 'MLSSelfRemovalNotAllowed
        :> CanThrow 'MLSStaleMessage
        :> CanThrow 'MLSSubConvClientNotInParent
        :> CanThrow 'MLSUnsupportedMessage
        :> CanThrow 'MLSUnsupportedProposal
        :> CanThrow MLSProposalFailure
        :> CanThrow NonFederatingBackends
        :> CanThrow UnreachableBackends
        :> "messages"
        :> ZLocalUser
        :> ZClient
        :> ZConn
        :> ReqBody '[MLS] (RawMLS Message)
        :> MultiVerb1 'POST '[JSON] (Respond 201 "Message sent" MLSMessageSendingStatus)
    )
    :<|> Named
           "mls-commit-bundle"
           ( Summary "Post a MLS CommitBundle"
               :> From 'V5
               :> MakesFederatedCall 'Galley "on-mls-message-sent"
               :> MakesFederatedCall 'Galley "mls-welcome"
               :> MakesFederatedCall 'Galley "send-mls-commit-bundle"
               :> MakesFederatedCall 'Galley "on-conversation-updated"
               :> MakesFederatedCall 'Brig "get-mls-clients"
               :> MakesFederatedCall 'Brig "get-users-by-ids"
               :> MakesFederatedCall 'Brig "api-version"
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvMemberNotFound
               :> CanThrow 'ConvNotFound
               :> CanThrow 'LegalHoldNotEnabled
               :> CanThrow 'MissingLegalholdConsent
               :> CanThrow 'MLSClientMismatch
               :> CanThrow 'MLSClientSenderUserMismatch
               :> CanThrow 'MLSCommitMissingReferences
               :> CanThrow 'MLSGroupConversationMismatch
               :> CanThrow 'MLSInvalidLeafNodeIndex
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'MLSProposalNotFound
               :> CanThrow 'MLSProtocolErrorTag
               :> CanThrow 'MLSSelfRemovalNotAllowed
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'MLSSubConvClientNotInParent
               :> CanThrow 'MLSUnsupportedMessage
               :> CanThrow 'MLSUnsupportedProposal
               :> CanThrow 'MLSWelcomeMismatch
               :> CanThrow MLSProposalFailure
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
               :> "commit-bundles"
               :> ZLocalUser
               :> ZClient
               :> ZConn
               :> ReqBody '[MLS] (RawMLS CommitBundle)
               :> MultiVerb1 'POST '[JSON] (Respond 201 "Commit accepted and forwarded" MLSMessageSendingStatus)
           )
    :<|> Named
           "mls-public-keys"
           ( Summary "Get public keys used by the backend to sign external proposals"
               :> From 'V5
               :> CanThrow 'MLSNotEnabled
               :> "public-keys"
               :> ZLocalUser
               :> MultiVerb1 'GET '[JSON] (Respond 200 "Public keys" MLSPublicKeys)
           )

type MLSAPI = LiftNamed ("mls" :> MLSMessagingAPI)
