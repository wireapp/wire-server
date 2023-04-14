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

module Wire.API.MLS.AuthenticatedContent
  ( AuthenticatedContent (..),
    authContentRef,
    publicMessageRef,
    mkSignedPublicMessage,
  )
where

import Crypto.PubKey.Ed25519
import qualified Data.ByteArray as BA
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Context
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation

-- | Needed to compute proposal refs.
-- https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-6-7
data AuthenticatedContent = AuthenticatedContent
  { wireFormat :: WireFormatTag,
    content :: RawMLS FramedContent,
    authData :: RawMLS FramedContentAuthData
  }
  deriving (Eq, Show)

instance SerialiseMLS AuthenticatedContent where
  serialiseMLS ac = do
    serialiseMLS ac.wireFormat
    serialiseMLS ac.content
    serialiseMLS ac.authData

msgAuthContent :: PublicMessage -> AuthenticatedContent
msgAuthContent msg =
  AuthenticatedContent
    { wireFormat = WireFormatPublicTag,
      content = msg.content,
      authData = msg.authData
    }

-- | Compute the proposal ref given a ciphersuite and the raw proposal data.
authContentRef :: CipherSuiteTag -> AuthenticatedContent -> ProposalRef
authContentRef cs = ProposalRef . csHash cs proposalContext . mkRawMLS

publicMessageRef :: CipherSuiteTag -> PublicMessage -> ProposalRef
publicMessageRef cs = authContentRef cs . msgAuthContent

-- | Craft a message with the backend itself as a sender. Return the message and its ref.
mkSignedPublicMessage ::
  SecretKey -> PublicKey -> GroupId -> Epoch -> FramedContentData -> PublicMessage
mkSignedPublicMessage priv pub gid epoch payload =
  let framedContent =
        mkRawMLS
          FramedContent
            { groupId = gid,
              epoch = epoch,
              sender = SenderExternal 0,
              content = payload,
              authenticatedData = mempty
            }
      tbs =
        FramedContentTBS
          { protocolVersion = defaultProtocolVersion,
            wireFormat = WireFormatPublicTag,
            content = framedContent,
            groupContext = Nothing
          }
      sig = BA.convert $ sign priv pub (encodeMLS' tbs)
   in PublicMessage
        { content = framedContent,
          authData = mkRawMLS (FramedContentAuthData sig Nothing),
          membershipTag = Nothing
        }
