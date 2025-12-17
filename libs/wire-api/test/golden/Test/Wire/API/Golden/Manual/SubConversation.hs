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

module Test.Wire.API.Golden.Manual.SubConversation
  ( testObject_PublicSubConversation_1,
    testObject_PublicSubConversation_2,
  )
where

import Data.Domain
import Data.Id
import Data.Qualified
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID qualified as UUID
import Imports
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Group
import Wire.API.MLS.SubConversation

subConvId1 :: SubConvId
subConvId1 = SubConvId "test_group"

subConvId2 :: SubConvId
subConvId2 = SubConvId "call"

domain :: Domain
domain = Domain "golden.example.com"

convId :: Qualified ConvId
convId =
  Qualified
    ( Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))
    )
    domain

testObject_PublicSubConversation_1 :: PublicSubConversation
testObject_PublicSubConversation_1 =
  PublicSubConversation
    convId
    subConvId1
    (GroupId "test_group")
    ( Just
        ( ActiveMLSConversationData
            (Epoch 5)
            (UTCTime day fromMidnight)
            MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
        )
    )
    []
  where
    fromMidnight :: DiffTime
    fromMidnight = 42
    day :: Day
    day = fromGregorian 2023 1 17

testObject_PublicSubConversation_2 :: PublicSubConversation
testObject_PublicSubConversation_2 =
  PublicSubConversation
    convId
    subConvId2
    (GroupId "test_group_2")
    Nothing
    [mkClientIdentity user cid]
  where
    user :: Qualified UserId
    user =
      Qualified
        ( Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000a00000002"))
        )
        domain
    cid = ClientId 0xdeadbeef
