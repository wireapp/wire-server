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

module Test.FeatureFlags where

import API.Galley
import qualified API.GalleyInternal as I
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import SetupHelpers
import Testlib.HTTP
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Helpers

data FeatureStatus = Disabled | Enabled
  deriving (Eq, Enum)

instance Show FeatureStatus where
  show = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

disabled, enabled :: String
disabled = "disabled"
enabled = "enabled"

expectedStatus :: HasCallStack => FeatureStatus -> Aeson.Value
expectedStatus fs =
  Aeson.object ["lockStatus" .= "unlocked", "status" .= show fs, "ttl" .= "unlimited"]

assertFeature ::
  (HasCallStack, MakesValue user, MakesValue team) =>
  String ->
  user ->
  team ->
  FeatureStatus ->
  App ()
assertFeature featureName user team expected = do
  tf <- getTeamFeature featureName user team
  assertSuccessMatchBody (expectedStatus expected) tf

assertFeatureFromAll ::
  (HasCallStack, MakesValue user) =>
  String ->
  user ->
  FeatureStatus ->
  App ()
assertFeatureFromAll featureName user expected = do
  actual <- extractTeamFeatureFromAll featureName user
  actual %. "status" `shouldMatch` show expected

assertFeatureInternal ::
  (HasCallStack, MakesValue domain) =>
  String ->
  domain ->
  String ->
  FeatureStatus ->
  App ()
assertFeatureInternal featureName dom team expected = do
  tf <- I.getTeamFeature dom featureName team
  assertSuccessMatchBody (expectedStatus expected) tf

--------------------------------------------------------------------------------

testLimitedEventFanout :: HasCallStack => App ()
testLimitedEventFanout = do
  let featureName = "limitedEventFanout"
  (_alice, team, _) <- createTeam OwnDomain 1
  assertFeatureInternal featureName OwnDomain team Disabled
  I.setTeamFeatureStatus OwnDomain team featureName "enabled"
  assertFeatureInternal featureName OwnDomain team Enabled

testSSOPut :: HasCallStack => App ()
testSSOPut = do
  let dom = OwnDomain
      featureName = "sso"
      setting = "settings.featureFlags." <> featureName
  (_alice, team, alex : _) <- createTeam dom 2
  nonMember <- randomUser dom def

  getTeamFeature featureName nonMember team `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

  configuredSSO <- readServiceConfig Galley & (%. setting) & asText
  if T.unpack configuredSSO == "disabled-by-default"
    then do
      assertFeature featureName alex team Disabled
      assertFeatureInternal featureName dom team Disabled
      assertFeatureFromAll featureName alex Disabled

      I.setTeamFeatureStatus OwnDomain team featureName "enabled"
      assertFeature featureName alex team Enabled
      assertFeatureInternal featureName dom team Enabled
      assertFeatureFromAll featureName alex Enabled
    else do
      assertFeature featureName alex team Enabled
      assertFeatureInternal featureName dom team Enabled
      assertFeatureFromAll featureName alex Enabled
