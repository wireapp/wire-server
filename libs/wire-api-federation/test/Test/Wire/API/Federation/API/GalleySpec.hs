{-# LANGUAGE QuasiQuotes #-}

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

module Test.Wire.API.Federation.API.GalleySpec where

import Data.Aeson
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Set as Set
import Imports
import Test.Hspec
import Test.Wire.API.Federation.API.Util
import Text.RawString.QQ
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Federation.API.Galley

spec :: Spec
spec = describe "Wire.API.Federation.API.Galley" $ do
  describe "RoundTripTests" $ do
    jsonRoundTrip @(ConversationCreated ConvId)

  describe "ConversationProtocol Aeson" $ do
    specConversationCreated

  describe "MLS send response timestamps" $ do
    let timestamp = toUTCTimeMillis (read "2026-11-11 10:54:51.693 UTC")
        legacySuccess = MLSMessageResponseUpdates []
        timedSuccess = MLSMessageResponseV5Success (MLSMessageSendSuccess [] timestamp)

    it "keeps the V0-V4 success format unchanged" $ do
      toJSON legacySuccess
        `shouldBe` object
          [ "tag" .= String "MLSMessageResponseUpdates",
            "contents" .= ([] :: [Value])
          ]

    it "round-trips legacy successes without a timestamp" $ do
      eitherDecode (encode legacySuccess) `shouldBe` Right legacySuccess

    it "preserves the required V5 timestamp" $ do
      eitherDecode (encode timedSuccess) `shouldBe` Right timedSuccess

    it "round-trips legacy errors" $ do
      let response = MLSMessageResponseProtocolError "invalid message"
      eitherDecode (encode response) `shouldBe` Right response

    it "requires the V5 success format with a timestamp" $ do
      let successContents fields = object ["tag" .= String "MLSMessageResponseV5Success", "contents" .= object ("updates" .= ([] :: [Value]) : fields)]
      for_ [toJSON legacySuccess, successContents [], successContents ["time" .= Null], successContents ["time" .= String "invalid"]] $ \value ->
        case fromJSON @MLSMessageResponseV5 value of
          Error _ -> pure ()
          Success _ -> expectationFailure "a V5 success without a timestamp was accepted"

    it "converts V5 successes to the legacy response at the V4 route" $ do
      mlsMessageResponseV5ToLegacy timedSuccess `shouldBe` legacySuccess

    it "preserves errors when converting V5 to V4" $ do
      let response = MLSMessageResponseProtocolError "invalid message"
      mlsMessageResponseV5ToLegacy (MLSMessageResponseV5Error response) `shouldBe` response

    it "round-trips V5 errors" $ do
      let response = MLSMessageResponseV5Error (MLSMessageResponseProtocolError "invalid message")
      eitherDecode (encode response) `shouldBe` Right response

specConversationCreated :: Spec
specConversationCreated = describe "ConversationCreated" $ do
  it "can parse ConversationCreated from JSON - omitted field group_conv_type and history are null" $ do
    let jsonStr =
          [r|{ "time": "2020-01-01T00:00:00Z",
      "orig_user_id": "00000000-0000-0000-0000-000000000000",
      "cnv_id": "00000001-0001-0001-0001-000000000001",
      "cnv_type": 0,
      "cnv_access": ["code", "private"],
      "cnv_access_roles": ["team_member"],
      "cnv_name": "Test Conv",
      "non_creator_members": [],
      "message_timer": null,
      "receipt_mode": null,
      "protocol": { "protocol": "proteus"}
    }|]
    let expected =
          ConversationCreated
            { time = read "2020-01-01 00:00:00 UTC",
              origUserId = Id $ read "00000000-0000-0000-0000-000000000000",
              cnvId = Id $ read "00000001-0001-0001-0001-000000000001",
              cnvType = RegularConv,
              cnvAccess = [CodeAccess, PrivateAccess],
              cnvAccessRoles = Set.singleton TeamMemberAccessRole,
              cnvName = Just "Test Conv",
              nonCreatorMembers = Set.empty,
              messageTimer = Nothing,
              receiptMode = Nothing,
              protocol = ProtocolProteus,
              groupConvType = Nothing,
              channelAddPermission = Nothing,
              history = Nothing
            }

    eitherDecode jsonStr `shouldBe` Right expected
