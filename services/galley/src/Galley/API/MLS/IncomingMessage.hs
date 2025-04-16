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

module Galley.API.MLS.IncomingMessage
  ( IncomingMessage (..),
    IncomingMessageContent (..),
    IncomingPublicMessageContent (..),
    IncomingBundle (..),
    SenderIdentity (..),
    mkIncomingMessage,
    incomingMessageAuthenticatedContent,
    mkIncomingBundle,
  )
where

import GHC.Records
import Imports
import Wire.API.MLS.AuthenticatedContent
import Wire.API.MLS.Commit
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

data IncomingMessage = IncomingMessage
  { epoch :: Epoch,
    groupId :: GroupId,
    content :: IncomingMessageContent,
    rawMessage :: RawMLS Message
  }

instance HasField "sender" IncomingMessage (Maybe Sender) where
  getField msg = case msg.content of
    IncomingMessageContentPublic pub -> Just pub.sender
    _ -> Nothing

data IncomingMessageContent
  = IncomingMessageContentPublic IncomingPublicMessageContent
  | IncomingMessageContentPrivate

data IncomingPublicMessageContent = IncomingPublicMessageContent
  { sender :: Sender,
    content :: FramedContentData,
    -- for verification
    framedContent :: RawMLS FramedContent,
    authData :: RawMLS FramedContentAuthData
  }

data IncomingBundle = IncomingBundle
  { epoch :: Epoch,
    groupId :: GroupId,
    sender :: Sender,
    commit :: RawMLS Commit,
    rawMessage :: RawMLS Message,
    welcome :: Maybe (RawMLS Welcome),
    groupInfo :: RawMLS GroupInfo,
    serialized :: ByteString
  }

-- | Client information about the sender of a message.
data SenderIdentity = SenderIdentity
  { -- | Sender client.
    client :: ClientIdentity,
    -- | Index of the client in the ratchet tree, if available.
    index :: Maybe LeafIndex
  }
  deriving (Show)

mkIncomingMessage :: RawMLS Message -> Maybe IncomingMessage
mkIncomingMessage msg = case msg.value.content of
  MessagePublic pmsg ->
    Just
      IncomingMessage
        { epoch = pmsg.content.value.epoch,
          groupId = pmsg.content.value.groupId,
          content =
            IncomingMessageContentPublic
              IncomingPublicMessageContent
                { sender = pmsg.content.value.sender,
                  content = pmsg.content.value.content,
                  framedContent = pmsg.content,
                  authData = pmsg.authData
                },
          rawMessage = msg
        }
  MessagePrivate pmsg
    | pmsg.value.tag == FramedContentApplicationDataTag ->
        Just
          IncomingMessage
            { epoch = pmsg.value.epoch,
              groupId = pmsg.value.groupId,
              content = IncomingMessageContentPrivate,
              rawMessage = msg
            }
  _ -> Nothing

incomingMessageAuthenticatedContent :: IncomingPublicMessageContent -> AuthenticatedContent
incomingMessageAuthenticatedContent pmsg =
  AuthenticatedContent
    { wireFormat = WireFormatPublicTag,
      content = pmsg.framedContent,
      authData = pmsg.authData
    }

mkIncomingBundle :: RawMLS CommitBundle -> Maybe IncomingBundle
mkIncomingBundle bundle = do
  imsg <- mkIncomingMessage bundle.value.commitMsg
  content <- case imsg.content of
    IncomingMessageContentPublic c -> pure c
    _ -> Nothing
  commit <- case content.content of
    FramedContentCommit c -> pure c
    _ -> Nothing
  pure
    IncomingBundle
      { epoch = imsg.epoch,
        groupId = imsg.groupId,
        sender = content.sender,
        commit = commit,
        rawMessage = bundle.value.commitMsg,
        welcome = bundle.value.welcome,
        groupInfo = bundle.value.groupInfo,
        serialized = bundle.raw
      }
