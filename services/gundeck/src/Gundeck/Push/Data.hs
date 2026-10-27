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

-- | Facade over the 'PushTokenStore' effect, selected by
-- @options.postgresMigration.pushTokens@. The queries themselves live in
-- "Wire.PushTokenStore.Cassandra" and "Wire.PushTokenStore.Postgres"; this
-- module converts between gundeck types ('EndpointArn') and the plain 'Text'
-- the store effect carries, and preserves the read-repair behavior of the
-- original implementation.
module Gundeck.Push.Data
  ( insert,
    updateArn,
    delete,
    lookup,
    erase,
    Consistency (..),
  )
where

import Cassandra
import Control.Error (hush)
import Control.Lens ((^.), view)
import Control.Monad.Catch (throwM)
import Data.ByteString.Conversion (toByteString)
import Data.Id (ClientId, ConnId, UserId)
import Gundeck.Aws.Arn (fromText, toText)
import Gundeck.Env
import Gundeck.Options (postgresMigration)
import Gundeck.Push.Native.Types
import Hasql.Pool (UsageError)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports hiding (lookup)
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputConst)
import System.IO.Error (userError)
import System.Logger qualified as Log
import System.Logger.Class (field, msg, val, (~~))
import Wire.API.Push.V2 hiding (pushTokens, token)
import Wire.PostgresMigrationOpts (PostgresMigrationOpts (..), StorageLocation (..))
import Wire.PushTokenStore qualified as Store
import Wire.PushTokenStore.Cassandra (interpretPushTokenStoreToCassandra)
import Wire.PushTokenStore.DualWrite (interpretPushTokenStoreToCassandraAndPostgres)
import Wire.PushTokenStore.Postgres (interpretPushTokenStoreToPostgres)

lookup :: (MonadReader Env m, MonadIO m) => UserId -> Consistency -> m [Address]
lookup u c = do
  rows <- runPushTokenStore $ Store.lookupPushTokens u c
  foldM mk [] rows
  where
    mk as r = maybe as (: as) <$> mkAddr r

insert :: (MonadReader Env m, MonadIO m) => UserId -> Transport -> AppName -> Token -> EndpointArn -> ConnId -> ClientId -> m ()
insert u t a p e o c =
  runPushTokenStore $ Store.insertPushToken u t a p (toText e) o c

updateArn :: (MonadReader Env m, MonadIO m) => UserId -> Transport -> AppName -> Token -> EndpointArn -> m ()
updateArn uid transport app token arn =
  runPushTokenStore $ Store.updatePushTokenArn uid transport app token (toText arn)

delete :: (MonadReader Env m, MonadIO m) => UserId -> Transport -> AppName -> Token -> m ()
delete u t a p =
  runPushTokenStore $ Store.deletePushToken u t a p

erase :: (MonadReader Env m, MonadIO m) => UserId -> m ()
erase u =
  runPushTokenStore $ Store.erasePushTokens u

-- | Builds an 'Address' from a stored row. Rows missing the client or the
-- ARN (or carrying an unparseable ARN) are logged and deleted: read repair,
-- preserved from the original implementation.
mkAddr :: (MonadReader Env m, MonadIO m) => Store.PushTokenRow -> m (Maybe Address)
mkAddr (usr, trp, app, tok, arn, conn, clt) = case (clt, arn >>= hush . fromText) of
  (Just clt', Just a) -> pure $! Just $! Address usr a conn (pushToken trp app tok clt')
  _ -> do
    env <- ask
    liftIO $
      Log.info (env ^. applog) $
        field "user" (toByteString usr)
          ~~ field "transport" (show trp)
          ~~ field "app" (appNameText app)
          ~~ field "token" (tokenText tok)
          ~~ msg (val "Deleting legacy push token without a client or ARN.")
    delete usr trp app tok
    pure Nothing

-- | Runs the push token store effect according to
-- @options.postgresMigration.pushTokens@, mirroring
-- "Gundeck.NotificationStoreBridge". 'Hasql.UsageError' from the Postgres
-- paths is rethrown as an exception, which the top-level error handler in
-- "Gundeck.Run" turns into a logged 500.
runPushTokenStore :: forall m a. (MonadReader Env m, MonadIO m) => Sem '[Store.PushTokenStore, Embed IO] a -> m a
runPushTokenStore sem = do
  env <- ask
  let pm = env ^. options . postgresMigration
      -- Grow the effect row behind PushTokenStore by inserting one
      -- effect at a time right after the head ('raiseUnder'), building the
      -- rows the store interpreters need.
      cassRow =
        raiseUnder (raiseUnder sem)
          :: Sem
               '[ Store.PushTokenStore,
                  Input ClientState,
                  Input HasqlPoolExt.Pool,
                  Embed IO
                ]
               a
      dualRow =
        raiseUnder cassRow
          :: Sem
               '[ Store.PushTokenStore,
                  Error UsageError,
                  Input ClientState,
                  Input HasqlPoolExt.Pool,
                  Embed IO
                ]
               a
      pgRow =
        raiseUnder
          ( raiseUnder sem
              :: Sem
                   '[ Store.PushTokenStore,
                      Input HasqlPoolExt.Pool,
                      Embed IO
                    ]
                   a
          )
          :: Sem
               '[ Store.PushTokenStore,
                  Error UsageError,
                  Input HasqlPoolExt.Pool,
                  Embed IO
                ]
               a
  liftIO $ do
    result <- runM $ case pushTokens pm of
      CassandraStorage ->
        -- reads and writes from Cassandra only
        pure . Right
          =<< runInputConst (view hasqlPool env)
            ( runInputConst (view cstate env)
                $ interpretPushTokenStoreToCassandra cassRow
            )
      MigrationToPostgresql ->
        -- writes mirrored to Postgres, reads from Cassandra
        runInputConst (view hasqlPool env)
          $ runInputConst (view cstate env)
          $ runError
          $ interpretPushTokenStoreToCassandraAndPostgres dualRow
      PostgresqlStorage ->
        -- reads and writes from Postgres only
        runInputConst (view hasqlPool env)
          $ runError
          $ interpretPushTokenStoreToPostgres pgRow
    either (throwM . userError . ("push token store: " <>) . show) pure result
