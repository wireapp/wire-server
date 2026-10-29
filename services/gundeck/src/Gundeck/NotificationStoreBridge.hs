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

-- | Bridge that runs the 'NotificationStore' effect inside the 'Gundeck'
-- monad (the first polysemy island in gundeck).
{-# LANGUAGE ScopedTypeVariables #-}

module Gundeck.NotificationStoreBridge
  ( runNotificationStore,
  )
where

import Control.Lens ((^.), view)
import Control.Monad.Catch (throwM)
import System.IO.Error (userError)
import Gundeck.Env
import Gundeck.Options (internalPageSize, maxPayloadLoadSize, postgresMigration, settings)
import Cassandra (ClientState)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Hasql.Pool (UsageError)
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputConst)
import Wire.NotificationStore
import Wire.NotificationStore.Cassandra (interpretNotificationStoreToCassandra)
import Wire.NotificationStore.DualWrite (interpretNotificationStoreToCassandraAndPostgres)
import Wire.NotificationStore.Postgres (interpretNotificationStoreToPostgres)
import Wire.PostgresMigrationOpts (PostgresMigrationOpts (..), StorageLocation (..))

-- | Interprets the notification store effect according to
-- @options.postgresMigration.notifications@. 'Hasql.UsageError' from the
-- Postgres paths is rethrown as an exception, which the top-level error
-- handler in "Gundeck.Run" turns into a logged 500.
runNotificationStore :: forall m a. (MonadReader Env m, MonadIO m) => Sem '[NotificationStore, Embed IO] a -> m a
runNotificationStore sem = do
  env <- ask
  let pm = env ^. options . postgresMigration
      cfg =
        NotificationStoreConfig
          (env ^. options . settings . internalPageSize)
          (env ^. options . settings . maxPayloadLoadSize)
      -- Grow the effect row behind NotificationStore by inserting one
      -- effect at a time right after the head ('raiseUnder'), building the
      -- rows the store interpreters need.
      cassRow =
        raiseUnder (raiseUnder (raiseUnder sem))
          :: Sem
               '[ NotificationStore,
                  Input ClientState,
                  Input NotificationStoreConfig,
                  Input HasqlPoolExt.Pool,
                  Embed IO
                ]
               a
      dualRow =
        raiseUnder cassRow
          :: Sem
               '[ NotificationStore,
                  Error UsageError,
                  Input ClientState,
                  Input NotificationStoreConfig,
                  Input HasqlPoolExt.Pool,
                  Embed IO
                ]
               a
      pgRow =
        raiseUnder
          ( raiseUnder (raiseUnder sem)
              :: Sem
                   '[ NotificationStore,
                      Input NotificationStoreConfig,
                      Input HasqlPoolExt.Pool,
                      Embed IO
                    ]
                   a
          )
          :: Sem
               '[ NotificationStore,
                  Error UsageError,
                  Input NotificationStoreConfig,
                  Input HasqlPoolExt.Pool,
                  Embed IO
                ]
               a
  liftIO $ do
    result <- runM $ case notifications pm of
      CassandraStorage ->
        -- reads and writes from Cassandra only
        Right <$> runInputConst (view hasqlPool env)
            ( runInputConst cfg
                $ runInputConst (view cstate env)
                $ interpretNotificationStoreToCassandra cassRow
            )
      MigrationToPostgresql ->
        -- writes mirrored to Postgres, reads from Cassandra
        runInputConst (view hasqlPool env)
          $ runInputConst cfg
          $ runInputConst (view cstate env)
          $ runError
          $ interpretNotificationStoreToCassandraAndPostgres dualRow
      PostgresqlStorage ->
        -- reads and writes from Postgres only
        runInputConst (view hasqlPool env)
          $ runInputConst cfg
          $ runError
          $ interpretNotificationStoreToPostgres pgRow
    either (throwM . userError . ("notification store: " <>) . show) pure result
