{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Test.Events where

import API.Galley
import qualified Control.Concurrent.Timeout as Timeout
import Control.Monad.RWS (asks)
import Data.Proxy (Proxy (..))
import qualified Data.Text as Text
import Network.AMQP.Extended (RabbitMqAdminOpts (vHost), mkRabbitMqAdminClientEnv)
import Network.RabbitMqAdmin
import Notifications
import Servant.API (AsApi, ToServant, toServant)
import Servant.API.Generic (fromServant)
import qualified Servant.Client as Servant
import SetupHelpers
import Testlib.Prelude

-- We only delete queues to clean up federated integration tests. So, we
-- mostly want to ensure we don't get stuck there.
testBackendPusherRecoversFromQueueDeletion :: (HasCallStack) => App ()
testBackendPusherRecoversFromQueueDeletion = do
  bob <- randomUser OwnDomain def

  domain1 <- asks (.domain1)

  let remotesRefreshInterval = 10000 :: Int
  startDynamicBackendsReturnResources
    [ enableConsumableNotifications
        ( def
            { backgroundWorkerCfg =
                setField
                  "backendNotificationPusher.remotesRefreshInterval"
                  remotesRefreshInterval
            }
        )
    ]
    $ \[beResource] -> do
      let domain = beResource.berDomain
      (alice, team, [alex, alison]) <- createTeam domain 3

      -- Create a federated conversion
      connectTwoUsers alice bob
      [alexId, alisonId, bobId] <-
        forM [alex, alison, bob] (%. "qualified_id")
      let nc = (defProteus {qualifiedUsers = [alexId, alisonId, bobId], team = Just team})
      void $ postConversation alice nc >>= getJSON 201

      withWebSockets [bob] $ \[wsBob] -> do
        rabbitmqAdminClient <- mkRabbitMqAdminClientForResource beResource
        let getActiveQueues :: App [String] =
              Text.unpack . (.name)
                <$$> ( (.items)
                         <$> rabbitmqAdminClient.listQueuesByVHost
                           (fromString beResource.berVHost)
                           (fromString "")
                           True
                           100
                           1
                     )

        void $ deleteTeamMember team alice alex >>= getBody 202

        assertConvUserDeletedNotif wsBob alexId

        -- Delete the queue
        let backendNotificationQueueName = "backend-notifications." <> domain1
        void
          $ rabbitmqAdminClient.deleteQueue
            (fromString beResource.berVHost)
            (fromString backendNotificationQueueName)

        -- Ensure the queue was deleted
        eventually $ do
          queueNames <- getActiveQueues
          queueNames `shouldNotContain` [backendNotificationQueueName]

        void $ deleteTeamMember team alice alison >>= getBody 202

        Timeout.threadDelay . fromIntegral $ remotesRefreshInterval * 2

        -- Check that the queue was recreated
        eventually $ do
          queueNames <- getActiveQueues
          queueNames `shouldContain` [backendNotificationQueueName]

        assertConvUserDeletedNotif wsBob alisonId

mkRabbitMqAdminClientForResource :: BackendResource -> App (AdminAPI (Servant.AsClientT App))
mkRabbitMqAdminClientForResource backend = do
  opts <- asks (.rabbitMQConfig)
  servantClient <- liftIO $ mkRabbitMqAdminClientEnv opts {vHost = Text.pack backend.berVHost}
  pure . fromServant $ Servant.hoistClient (Proxy @(ToServant AdminAPI AsApi)) (liftIO @App) (toServant servantClient)

enableConsumableNotifications :: ServiceOverrides -> ServiceOverrides
enableConsumableNotifications overrides =
  overrides
    <> def
      { brigCfg = setField "optSettings.setConsumableNotifications" True,
        gundeckCfg = setField "settings.consumableNotifications" True
      }
