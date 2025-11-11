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

module Test.FeatureFlags.SelfDeletingMessages where

import qualified Data.Aeson.Types as A
import Test.FeatureFlags.Util
import Testlib.Prelude

feature :: (ToJSON timeout) => [A.Pair] -> timeout -> Value
feature ps timeout =
  object
    ( ps
        <> [ "ttl" .= "unlimited",
             "config" .= object ["enforcedTimeoutSeconds" .= toJSON timeout]
           ]
    )

testSelfDeletingMessages :: (HasCallStack) => APIAccess -> App ()
testSelfDeletingMessages access =
  mkFeatureTests "selfDeletingMessages"
    & addUpdate (feature ["status" .= "disabled"] (0 :: Int))
    & addUpdate (feature ["status" .= "enabled"] (30 :: Int))
    & addInvalidUpdate (feature ["status" .= "enabled"] "")
    & runFeatureTests OwnDomain access

testPatchSelfDeletingMessages :: (HasCallStack) => App ()
testPatchSelfDeletingMessages = do
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "locked"]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["status" .= "disabled"]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["lockStatus" .= "unlocked", "config" .= object ["enforcedTimeoutSeconds" .= A.Number 30]]
  checkPatch OwnDomain "selfDeletingMessages"
    $ object ["config" .= object ["enforcedTimeoutSeconds" .= A.Number 60]]
