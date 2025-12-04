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

module Test.FeatureFlags.CellsInternal where

import qualified API.GalleyInternal as Internal
import SetupHelpers
import Test.Cells (QueueConsumer (..), getMessage, watchCellsEvents)
import Test.FeatureFlags.Util
import Testlib.Prelude

testCellsInternalEvent :: (HasCallStack) => App ()
testCellsInternalEvent = do
  (alice, tid, _) <- createTeam OwnDomain 0
  q <- do
    q <- watchCellsEvents def
    let isEventForTeam v = fieldEquals @Value v "payload.0.team" tid
    -- the cells event queue is shared by tests
    -- let's hope this filter reduces the risk of tests interfering with eaach other
    pure $ q {filter = isEventForTeam}
  let quota = "234723984"
      update = mkFt "enabled" "unlocked" defConf {quota}
  setFeature InternalAPI alice tid "cellsInternal" update >>= assertSuccess
  event <- getMessage q %. "payload.0"
  event %. "name" `shouldMatch` "cellsInternal"
  event %. "team" `shouldMatch` tid
  event %. "type" `shouldMatch` "feature-config.update"
  event %. "data.lockStatus" `shouldMatch` "unlocked"
  event %. "data.status" `shouldMatch` "enabled"
  event %. "data.config.backend.url" `shouldMatch` "https://cells-beta.wire.com"
  event %. "data.config.collabora.edition" `shouldMatch` "CODE"
  event %. "data.config.storage.teamQuotaBytes" `shouldMatch` quota

testCellsInternal :: (HasCallStack) => App ()
testCellsInternal = do
  (alice, tid, _) <- createTeam OwnDomain 0

  withWebSocket alice $ \ws -> do
    for_ validCellsInternlUpdates $ setFlag InternalAPI ws tid "cellsInternal"
    for_ invalidCellsInternalUpdates $ setFeature InternalAPI alice tid "cellsInternal" >=> getJSON 400

  -- the feature does not have a public PUT endpoint
  setFeature PublicAPI alice tid "cellsInternal" enabled `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404
    resp.json %. "label" `shouldMatch` "no-endpoint"

validCellsInternlUpdates :: [Value]
validCellsInternlUpdates =
  [ mkFt "enabled" "unlocked" defConf,
    mkFt "enabled" "unlocked" defConf {collabora = "NO"},
    mkFt "enabled" "unlocked" defConf {collabora = "COOL"},
    mkFt "enabled" "unlocked" defConf {url = "https://wire.com"},
    mkFt "enabled" "unlocked" defConf {quota = "92346832946243"}
  ]

invalidCellsInternalUpdates :: [Value]
invalidCellsInternalUpdates =
  [ mkFt "enabled" "unlocked" defConf {collabora = "FOO"},
    mkFt "enabled" "unlocked" defConf {url = "http://wire.com"},
    mkFt "enabled" "unlocked" defConf {quota = "-92346832946243"},
    mkFt "enabled" "unlocked" defConf {quota = "1 TB"},
    mkFt "disabled" "unlocked" defConf
  ]

mkFt :: String -> String -> CellsInternalConfig -> Value
mkFt s ls c =
  object
    [ "lockStatus" .= ls,
      "status" .= s,
      "ttl" .= "unlimited",
      "config"
        .= object
          [ "backend" .= object ["url" .= c.url],
            "collabora" .= object ["edition" .= c.collabora],
            "storage" .= object ["teamQuotaBytes" .= c.quota]
          ]
    ]

defConf :: CellsInternalConfig
defConf =
  CellsInternalConfig
    { url = "https://cells-beta.wire.com",
      collabora = "CODE",
      quota = "1000000000000"
    }

testPatchCellsInternal :: (HasCallStack) => App ()
testPatchCellsInternal = do
  for_ validCellsInternlUpdates $ checkPatch OwnDomain "cellsInternal"
  (_, tid, _) <- createTeam OwnDomain 0
  for_ (mkFt "enabled" "locked" defConf : invalidCellsInternalUpdates)
    $ Internal.patchTeamFeature OwnDomain tid "cellsInternal"
    >=> assertStatus 400

data CellsInternalConfig = CellsInternalConfig
  { url :: String,
    collabora :: String,
    quota :: String
  }
