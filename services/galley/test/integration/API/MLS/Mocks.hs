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

module API.MLS.Mocks
  ( receiveCommitMock,
    receiveCommitMockByDomain,
    messageSentMock,
    welcomeMock,
    sendMessageMock,
    claimKeyPackagesMock,
    queryGroupStateMock,
  )
where

import Data.Id
import Data.Json.Util
import Data.Qualified
import qualified Data.Set as Set
import Federator.MockServer
import Imports
import Wire.API.Error.Galley
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.User.Client

receiveCommitMock :: [ClientIdentity] -> Mock LByteString
receiveCommitMock clients =
  asum
    [ "on-conversation-updated" ~> (),
      "on-new-remote-conversation" ~> EmptyResponse,
      "get-mls-clients" ~>
        Set.fromList
          ( map (flip ClientInfo True . ciClient) clients
          )
    ]

receiveCommitMockByDomain :: [ClientIdentity] -> Mock LByteString
receiveCommitMockByDomain clients = do
  r <- getRequest
  let fClients = filter (\c -> frTargetDomain r == ciDomain c) clients
  asum
    [ "on-conversation-updated" ~> (),
      "on-new-remote-conversation" ~> EmptyResponse,
      "get-mls-clients" ~>
        Set.fromList
          ( map (flip ClientInfo True . ciClient) fClients
          )
    ]

messageSentMock :: Mock LByteString
messageSentMock = "on-mls-message-sent" ~> RemoteMLSMessageOk

welcomeMock :: Mock LByteString
welcomeMock = "mls-welcome" ~> MLSWelcomeSent

sendMessageMock :: Mock LByteString
sendMessageMock =
  "send-mls-message" ~>
    MLSMessageResponseUpdates
      []
      (UnreachableUserList [])

claimKeyPackagesMock :: KeyPackageBundle -> Mock LByteString
claimKeyPackagesMock kpb = "claim-key-packages" ~> kpb

queryGroupStateMock :: ByteString -> Qualified UserId -> Mock LByteString
queryGroupStateMock gs qusr = do
  guardRPC "query-group-info"
  uid <- ggireqSender <$> getRequestBody
  mockReply $
    if uid == qUnqualified qusr
      then GetGroupInfoResponseState (Base64ByteString gs)
      else GetGroupInfoResponseError ConvNotFound
