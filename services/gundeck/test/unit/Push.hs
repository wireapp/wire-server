{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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

module Push where

import Data.Aeson qualified as Aeson
import Gundeck.Push (pushAll, pushAny)
import Gundeck.Push.Websocket as Web (bulkPush)
import Imports
import MockGundeck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Internal.Notification
import Wire.API.Presence
import Wire.API.Push.V2

tests :: TestTree
tests =
  testGroup
    "bulkpush"
    [ testProperty "web sockets" webBulkPushProps,
      testProperty "native pushes" pushAllProps
    ]

mkEnv :: (Pretty MockEnv -> Property) -> Positive Int -> Property
mkEnv prop (Positive len) = forAllShrink (Pretty <$> resize len genMockEnv) (shrinkPretty shrinkMockEnv) prop

webBulkPushProps :: Positive Int -> Property
webBulkPushProps plen@(Positive len) = mkEnv mkNotifs plen
  where
    mkNotifs :: Pretty MockEnv -> Property
    mkNotifs (Pretty env) =
      forAllShrink
        (Pretty <$> resize len (genNotifs env))
        (shrinkPretty shrinkNotifs)
        (webBulkPushProp env)

webBulkPushProp :: MockEnv -> Pretty [(Notification, [Presence])] -> Property
webBulkPushProp env (Pretty notifs) =
  counterexample "^ environment, notifications\n" $
    conjoin props
  where
    (realout, realst) = runMockGundeck env $ Web.bulkPush notifs
    (mockout, mockst) = runMockGundeck env $ mockBulkPush notifs
    props =
      [ realst === mockst,
        sort realout === sort mockout
      ]

pushAllProps :: Positive Int -> Property
pushAllProps plen@(Positive len) = mkEnv mkPushes plen
  where
    mkPushes :: Pretty MockEnv -> Property
    mkPushes (Pretty env) =
      forAllShrink
        (Pretty <$> resize len (genPushes env))
        (shrinkPretty shrinkPushes)
        (pushAllProp env)

pushAllProp :: MockEnv -> Pretty [Push] -> Property
pushAllProp env (Pretty pushes) =
  counterexample "^ environment, pushes\n" $
    conjoin props
  where
    ((), realst) = runMockGundeck env (pushAll pushes)
    ((), mockst) = runMockGundeck env (mockPushAll pushes)
    (errs, oldst) = runMockGundeck env (pushAny pushes)
    props =
      [ (Aeson.eitherDecode . Aeson.encode) pushes === Right pushes,
        (Aeson.eitherDecode . Aeson.encode) env === Right env,
        counterexample "real vs. mock:" $ realst === mockst,
        counterexample "real vs. old:" $ realst === oldst,
        counterexample "old errors:" $ isRight errs === True
      ]
