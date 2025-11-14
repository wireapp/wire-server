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

module Test.FeatureFlags.ConferenceCalling where

import qualified Data.Aeson as A
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchConferenceCalling :: (HasCallStack) => App ()
testPatchConferenceCalling = do
  checkPatchWithComputeExpected OwnDomain "conferenceCalling" (object ["lockStatus" .= "locked"]) computeExpectedValue
  checkPatchWithComputeExpected OwnDomain "conferenceCalling" (object ["status" .= "disabled"]) computeExpectedValue
  checkPatchWithComputeExpected OwnDomain "conferenceCalling" (object ["lockStatus" .= "locked", "status" .= "disabled"]) computeExpectedValue
  checkPatchWithComputeExpected OwnDomain "conferenceCalling" (object ["lockStatus" .= "unlocked", "config" .= object ["useSFTForOneToOneCalls" .= toJSON True]]) computeExpectedValue
  where
    computeExpectedValue :: String -> Value -> Value -> App Value
    computeExpectedValue "status" _ patch = do
      statusFromPatch <- lookupField patch "status"
      -- conference calling behaves differently than other features
      -- if unlocked, the status is set to enabled even thought the default is disabled
      maybe (pure (A.String (fromString "enabled"))) pure statusFromPatch
    computeExpectedValue key defFeature patch = do
      mValue <- lookupField patch key
      maybe (defFeature %. key) pure mValue

testConferenceCalling :: (HasCallStack) => APIAccess -> App ()
testConferenceCalling access = do
  runFeatureTests OwnDomain access
    $ mkFeatureTests "conferenceCalling"
    & addUpdate (confCalling def {sft = toJSON True})
    & addUpdate (confCalling def {sft = toJSON False})
    & addInvalidUpdate (confCalling def {sft = toJSON (0 :: Int)})
