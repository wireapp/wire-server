-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.MLS.History where

import API.Galley
import qualified API.GalleyInternal as I
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as T
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testExtraAppMessage :: App ()
testExtraAppMessage = do
  [alice, bob, charlie] <- createAndConnectUsers (replicate 3 OwnDomain)
  [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
  traverse_ (uploadNewKeyPackage def) [bob1, charlie1]
  convId <- createNewGroup def alice1

  -- normal commit
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  -- make a commit with an extra application message
  mp <- createAddCommit alice1 convId [charlie]
  appPackage <- createApplicationMessage convId alice1 "hello"
  let mp' = mp {appMessage = Just appPackage.message}

  withWebSockets [bob1, charlie1] $ \wss -> do
    void $ sendAndConsumeCommitBundle mp'

    let isAppMessage :: Value -> App Bool
        isAppMessage n =
          isNewMLSMessageNotif n
            &&~ isNotifConvId mp.convId n
            &&~ ( do
                    msg <- n %. "payload.0.data" & asByteString >>= showMessage def alice1
                    ty <- msg %. "type" & asString
                    pure $ ty == "private_message"
                )

    for_ wss $ \ws -> do
      n <- awaitMatch isAppMessage ws
      nPayload n %. "data" `shouldMatch` T.decodeUtf8 (Base64.encode appPackage.message)

testConvCreateWithHistory :: App ()
testConvCreateWithHistory = do
  (alice, tid, _) <- createTeam OwnDomain 1

  I.setTeamFeatureLockStatus alice tid "channels" "unlocked"
  setTeamFeatureConfig alice tid "channels" channelsConfig >>= assertSuccess

  let history = object ["depth" .= "infinite"]
  bindResponse
    ( postConversation
        alice
        ( defMLS
            { team = Just tid,
              history = Just history
            }
        )
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 400
      resp.json %. "label" `shouldMatch` "history-not-supported"

  convId <- bindResponse
    ( postConversation
        alice
        ( defMLS
            { team = Just tid,
              history = Just history,
              groupConvType = Just "channel"
            }
        )
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 201
      objConvId resp.json

  conv <- getConversation alice convId >>= getJSON 200
  conv %. "history" `shouldMatch` history

testRegularConvCannotSetHistory :: App ()
testRegularConvCannotSetHistory = do
  alice <- randomUser OwnDomain def
  let history = object ["depth" .= "infinite"]
  convId <- postConversation alice defMLS >>= getJSON 201 >>= objConvId

  bindResponse (updateHistory alice convId history) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "history-not-supported"

channelsConfig :: Value
channelsConfig =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "allowed_to_create_channels" .= "team-members",
            "allowed_to_open_channels" .= "team-members"
          ]
    ]
