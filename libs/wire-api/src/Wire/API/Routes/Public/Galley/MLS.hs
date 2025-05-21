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

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Servant
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.Version
import Wire.Arbitrary

data MLSReset = MLSReset
  { groupId :: GroupId,
    epoch :: Epoch
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MLSReset
  deriving (Arbitrary) via (GenericUniform MLSReset)

instance ToSchema MLSReset where
  schema =
    object "MLSReset" $
      MLSReset
        <$> (.groupId) .= field "group_id" schema
        <*> (.epoch) .= field "epoch" schema

type MLSMessagingAPI =
  Named
    "mls-message"
    ( Summary "Post an MLS message"
        :> From 'V5
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
               :> From V5
               :> CanThrow ConvAccessDenied
               :> CanThrow ConvMemberNotFound
               :> CanThrow ConvNotFound
               :> CanThrow LegalHoldNotEnabled
               :> CanThrow MissingLegalholdConsent
               :> CanThrow MLSClientMismatch
               :> CanThrow MLSClientSenderUserMismatch
               :> CanThrow MLSCommitMissingReferences
               :> CanThrow MLSGroupConversationMismatch
               :> CanThrow MLSInvalidLeafNodeIndex
               :> CanThrow MLSNotEnabled
               :> CanThrow MLSProposalNotFound
               :> CanThrow MLSProtocolErrorTag
               :> CanThrow MLSSelfRemovalNotAllowed
               :> CanThrow MLSStaleMessage
               :> CanThrow MLSSubConvClientNotInParent
               :> CanThrow MLSUnsupportedMessage
               :> CanThrow MLSUnsupportedProposal
               :> CanThrow MLSWelcomeMismatch
               :> CanThrow MLSLegalholdIncompatible
               :> CanThrow MLSProposalFailure
               :> CanThrow MLSIdentityMismatch
               :> CanThrow NonFederatingBackends
               :> CanThrow UnreachableBackends
               :> "commit-bundles"
               :> ZLocalUser
               :> ZClient
               :> ZConn
               :> ReqBody '[MLS] (RawMLS CommitBundle)
               :> MultiVerb1 POST '[JSON] (Respond 201 "Commit accepted and forwarded" MLSMessageSendingStatus)
           )
    :<|> Named
           "mls-public-keys"
           ( Summary "Get public keys used by the backend to sign external proposals"
               :> Description
                    "The format of the returned key is determined by the `format` query parameter:\n\
                    \ - raw (default): base64-encoded raw public keys\n\
                    \ - jwk: keys are nested objects in JWK format."
               :> From 'V5
               :> CanThrow 'MLSNotEnabled
               :> "public-keys"
               :> ZLocalUser
               :> QueryParam "format" MLSPublicKeyFormat
               :> MultiVerb1
                    'GET
                    '[JSON]
                    ( Respond
                        200
                        "Public keys"
                        (MLSKeysByPurpose (MLSKeys SomeKey))
                    )
           )
    :<|> Named
           "mls-reset-conversation"
           ( Summary "Reset an MLS conversation to epoch 0"
               :> "reset-conversation"
               :> ZLocalUser
               :> ReqBody '[JSON] MLSReset
               :> CanThrow 'MLSNotEnabled
               :> CanThrow 'MLSStaleMessage
               :> CanThrow 'ConvAccessDenied
               :> CanThrow 'ConvNotFound
               :> CanThrow 'InvalidOperation
               :> CanThrow 'MLSFederatedResetNotSupported
               :> CanThrow MLSProtocolErrorTag
               :> MultiVerb1
                    'POST
                    '[JSON]
                    (RespondEmpty 200 "Conversation reset")
           )

type MLSAPI = LiftNamed ("mls" :> MLSMessagingAPI)
