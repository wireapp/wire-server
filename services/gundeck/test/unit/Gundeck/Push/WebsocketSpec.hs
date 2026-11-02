-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.Push.WebsocketSpec (tests) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Gundeck.Push.Websocket (MonadBulkPush (..), bulkPush)
import Imports
import MockGundeck (MockEnv, MockGundeck, Pretty (..), genMockEnv, genNotifs, runMockGundeck, shrinkNotifs)
import System.Logger.Class qualified as Log
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.Internal.Notification
import Wire.API.Presence

tests :: TestTree
tests =
  testGroup
    "websocket"
    [testProperty "bulkPush reports its deliveries to the monitor" bulkPushMonitorsProp]

-- | What 'bulkPush' reported to the monitoring methods of 'MonadBulkPush'.
data Monitored
  = BadCannons [Presence]
  | Delivered [(Notification, [Presence])] [(NotificationId, Presence)]

-- | 'MockGundeck' that records the calls of the monitoring methods.
newtype Recording a = Recording {unRecording :: WriterT [Monitored] MockGundeck a}
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadWriter [Monitored])

instance Log.MonadLogger Recording where
  log lvl msg = Recording (lift (Log.log lvl msg))

instance MonadBulkPush Recording where
  mbpBulkSend uri req = Recording (lift (mbpBulkSend uri req))
  mbpDeleteAllPresences = Recording . lift . mbpDeleteAllPresences
  mbpPosixTime = Recording (lift mbpPosixTime)
  mbpMapConcurrently = mapM
  mbpMonitorBadCannons (_, (_, prcs)) = tell [BadCannons prcs]
  mbpMonitorDelivered notifs successes = tell [Delivered notifs successes]

-- | An environment, and notifications for the presences in it. They can only
-- be generated (and shrunk) together.
data EnvAndNotifs = EnvAndNotifs MockEnv [(Notification, [Presence])]

instance Show EnvAndNotifs where
  show (EnvAndNotifs env notifs) = show (Pretty env) <> "\n" <> show (Pretty notifs)

instance Arbitrary EnvAndNotifs where
  arbitrary = do
    env <- genMockEnv
    EnvAndNotifs env <$> genNotifs env
  shrink (EnvAndNotifs env notifs) = EnvAndNotifs env <$> shrinkNotifs notifs

bulkPushMonitorsProp :: EnvAndNotifs -> Property
bulkPushMonitorsProp (EnvAndNotifs env notifs) =
  conjoin
    [ -- The mock has no unreachable cannons.
      badCannons === [],
      case deliveries of
        [(notifs', successes)] ->
          notifs' === notifs .&&. length successes === sum (length . snd <$> out)
        _ -> counterexample "expected exactly one Delivered call" False
    ]
  where
    badCannons = [prcs | BadCannons prcs <- monitored]
    deliveries = [(notifs', successes) | Delivered notifs' successes <- monitored]
    ((out, monitored), _) = runMockGundeck env (runWriterT (unRecording (bulkPush notifs)))
