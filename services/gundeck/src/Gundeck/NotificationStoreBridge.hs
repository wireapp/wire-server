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
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bridge that runs the 'NotificationStore' effect inside the 'Gundeck'
-- monad (the first polysemy island in gundeck).
module Gundeck.NotificationStoreBridge
  ( runNotificationStore,
  )
where

import Cassandra (ClientState)
import Control.Lens (view, (^.))
import Control.Monad.Catch (throwM)
import Gundeck.Env
import Gundeck.Options (internalPageSize, maxPayloadLoadSize, postgresMigration, settings)
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Async (Async, asyncToIOFinal)
import Polysemy.Conc (interpretRace)
import Polysemy.Conc.Effect.Race (Race)
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputConst)
import Polysemy.Resource (Resource, resourceToIOFinal)
import Polysemy.TinyLog (TinyLog)
import System.IO.Error (userError)
import Wire.NotificationStore
import Wire.NotificationStore.Cassandra (interpretNotificationStoreToCassandra)
import Wire.NotificationStore.DualWrite (interpretNotificationStoreToCassandraAndPostgres)
import Wire.NotificationStore.Postgres (interpretNotificationStoreToPostgres)
import Wire.PostgresMigrationOpts (PostgresMigrationOpts (..), StorageLocation (..))
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)

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
        raiseUnder (raiseUnder (raiseUnder sem)) ::
          Sem
            '[ NotificationStore,
               Input ClientState,
               Input NotificationStoreConfig,
               Input HasqlPoolExt.Pool,
               Embed IO
             ]
            a
      -- The dual-write interpreter takes shared per-user migration locks
      -- around dual deletes and the Postgres add leg, so its row
      -- additionally carries the lock machinery (TinyLog/Async/Race/Resource).
      -- 'insertAt' appends 'Final IO' (needed by the *ToFinal interpreters)
      -- at the end of the row, then 'raiseUnder' inserts the lock effects
      -- right behind the head, one at a time.
      dualRow =
        raiseUnder
          ( raiseUnder
              ( raiseUnder
                  ( raiseUnder
                      (raiseUnder (insertAt @5 @'[Final IO] cassRow))
                  )
              )
          ) ::
          Sem
            '[ NotificationStore,
               TinyLog,
               Async,
               Race,
               Resource,
               Error UsageError,
               Input ClientState,
               Input NotificationStoreConfig,
               Input HasqlPoolExt.Pool,
               Embed IO,
               Final IO
             ]
            a
      pgRow =
        raiseUnder
          ( raiseUnder (raiseUnder sem) ::
              Sem
                '[ NotificationStore,
                   Input NotificationStoreConfig,
                   Input HasqlPoolExt.Pool,
                   Embed IO
                 ]
                a
          ) ::
          Sem
            '[ NotificationStore,
               Error UsageError,
               Input NotificationStoreConfig,
               Input HasqlPoolExt.Pool,
               Embed IO
             ]
            a
  liftIO $ do
    result <- case notifications pm of
      CassandraStorage ->
        -- reads and writes from Cassandra only
        Right
          <$> runM
            ( runInputConst
                (view hasqlPool env)
                ( runInputConst cfg $
                    runInputConst (view cstate env) $
                      interpretNotificationStoreToCassandra cassRow
                )
            )
      MigrationToPostgresql ->
        -- writes mirrored to Postgres, reads from Cassandra
        runFinal
          . embedToFinal
          . runInputConst (view hasqlPool env)
          . runInputConst cfg
          . runInputConst (view cstate env)
          . runError
          . resourceToIOFinal
          . interpretRace
          . asyncToIOFinal
          . loggerToTinyLog (view applog env)
          $ interpretNotificationStoreToCassandraAndPostgres dualRow
      PostgresqlStorage ->
        -- reads and writes from Postgres only
        runM
          ( runInputConst
              (view hasqlPool env)
              ( runInputConst cfg $
                  runError $
                    interpretNotificationStoreToPostgres pgRow
              )
          )
    either (throwM . userError . ("notification store: " <>) . show) pure result
