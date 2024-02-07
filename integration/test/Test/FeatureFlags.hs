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
import SetupHelpers
import Testlib.HTTP
import Testlib.Prelude

--------------------------------------------------------------------------------
-- Helpers

data FeatureStatus = Disabled | Enabled
  deriving (Eq)

instance HasTests x => HasTests (FeatureStatus -> x) where
  mkTests m n s f x =
    mkTests m (n <> "[featureStatus=disabled]") s f (x Disabled)
      <> mkTests m (n <> "[featureStatus=enabled]") s f (x Enabled)

instance Show FeatureStatus where
  show = \case
    Disabled -> "disabled"
    Enabled -> "enabled"

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

testSSOPut :: HasCallStack => FeatureStatus -> App ()
testSSOPut status = withModifiedBackend cnf $ \domain -> do
  (_alice, team, alex : _) <- createTeam domain 2
  nonMember <- randomUser domain def

  getTeamFeature featureName nonMember team `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

  assertFeature featureName alex team status
  assertFeatureInternal featureName domain team status
  assertFeatureFromAll featureName alex status

  when (status == Disabled) $ do
    let opposite = Enabled
    I.setTeamFeatureStatus domain team featureName (show opposite)
    assertFeature featureName alex team opposite
    assertFeatureInternal featureName domain team opposite
    assertFeatureFromAll featureName alex opposite
  where
    featureName = "sso"
    setting = "settings.featureFlags." <> featureName
    cnf =
      def
        { galleyCfg = \conf ->
            conf
              & setField setting (show status <> "-by-default")
        }
