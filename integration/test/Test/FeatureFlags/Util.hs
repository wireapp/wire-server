-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.FeatureFlags.Util where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import Testlib.Prelude

disabled :: Value
disabled = object ["lockStatus" .= "unlocked", "status" .= "disabled", "ttl" .= "unlimited"]

disabledLocked :: Value
disabledLocked = object ["lockStatus" .= "locked", "status" .= "disabled", "ttl" .= "unlimited"]

enabled :: Value
enabled = object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited"]

checkFeature :: (HasCallStack, MakesValue user, MakesValue tid) => String -> user -> tid -> Value -> App ()
checkFeature = checkFeatureWith shouldMatch

checkFeatureWith :: (HasCallStack, MakesValue user, MakesValue tid, MakesValue expected) => ((HasCallStack) => App Value -> expected -> App ()) -> String -> user -> tid -> expected -> App ()
checkFeatureWith shouldMatch' feature user tid expected = do
  tidStr <- asString tid
  domain <- objDomain user
  bindResponse (Internal.getTeamFeature domain tidStr feature) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch'` expected
  bindResponse (Public.getTeamFeatures user tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. feature `shouldMatch'` expected
  bindResponse (Public.getTeamFeature user tid feature) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch'` expected
  bindResponse (Public.getFeatureConfigs user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. feature `shouldMatch'` expected

assertForbidden :: (HasCallStack) => Response -> App ()
assertForbidden = assertLabel 403 "no-team-member"

data ConfCalling = ConfCalling
  { lockStatus :: Maybe String,
    status :: String,
    sft :: Value
  }

instance Default ConfCalling where
  def =
    ConfCalling
      { lockStatus = Nothing,
        status = "disabled",
        sft = toJSON False
      }

confCalling :: ConfCalling -> Value
confCalling args =
  object
    $ ["lockStatus" .= s | s <- toList args.lockStatus]
    <> ["ttl" .= "unlimited"]
    <> [ "status" .= args.status,
         "config"
           .= object ["useSFTForOneToOneCalls" .= args.sft]
       ]
