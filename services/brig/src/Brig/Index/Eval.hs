{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Index.Eval
  ( runCommand,
  )
where

import Brig.Index.Migrations
import Brig.Index.Options
import Brig.User.Search.Index
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens
import qualified Control.Lens.Internal.ByteString as LensBS
import Control.Monad.Catch
import Control.Retry
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Metrics as Metrics
import qualified Database.Bloodhound as ES
import Imports
import Network.HTTP.Client as HTTP
import qualified System.Logger as Log
import System.Logger.Class (Logger, MonadLogger (..))

runCommand :: Logger -> Command -> IO ()
runCommand l = \case
  Create es -> do
    e <- initIndex es
    runIndexIO e $ uncurry createIndexIfNotPresent $ mkCreateIndexSettings es
  Reset es -> do
    e <- initIndex es
    runIndexIO e $ uncurry resetIndex $ mkCreateIndexSettings es
  Reindex es cas -> do
    e <- initIndex es
    c <- initDb cas
    runReindexIO e c reindexAll
  ReindexSameOrNewer es cas -> do
    e <- initIndex es
    c <- initDb cas
    runReindexIO e c reindexAllIfSameOrNewer
  UpdateMapping esURI indexName -> do
    e <- initIndex' esURI indexName
    runIndexIO e updateMapping
  Migrate es cas -> do
    migrate l es cas
  ReindexFromAnotherIndex reindexSettings -> do
    bhEnv <- initES (view reindexEsServer reindexSettings)
    ES.runBH bhEnv $ do
      let src = view reindexSrcIndex reindexSettings
          dest = view reindexDestIndex reindexSettings
          timeoutSeconds = view reindexTimeoutSeconds reindexSettings

      srcExists <- ES.indexExists src
      unless srcExists $ do
        throwM $ ReindexFromAnotherIndexError $ "Source index " <> show src <> " doesn't exist"

      destExists <- ES.indexExists dest
      unless destExists $ do
        throwM $ ReindexFromAnotherIndexError $ "Destination index " <> show dest <> " doesn't exist"

      Log.info l $ Log.msg ("Reindexing" :: ByteString) . Log.field "from" (show src) . Log.field "to" (show dest)
      eitherTaskNodeId <- ES.reindexAsync $ ES.mkReindexRequest src dest
      case eitherTaskNodeId of
        Left err -> throwM $ ReindexFromAnotherIndexError $ "Error occurred while running reindex: " <> show err
        Right taskNodeId -> do
          Log.info l $ Log.field "taskNodeId" (show taskNodeId)
          waitForTaskToComplete @ES.ReindexResponse timeoutSeconds taskNodeId
          Log.info l $ Log.msg ("Finished reindexing" :: ByteString)
  where
    initIndex es =
      initIndex' (es ^. esServer) (es ^. esIndex)
    initIndex' esURI indexName =
      IndexEnv
        <$> Metrics.metrics
        <*> pure l
        <*> initES esURI
        <*> pure Nothing
        <*> pure indexName
        <*> pure Nothing
    initES esURI =
      ES.mkBHEnv (toESServer esURI)
        <$> newManager defaultManagerSettings
    initDb cas =
      C.init $
        C.setLogger (C.mkLogger l)
          . C.setContacts (view cHost cas) []
          . C.setPortNumber (fromIntegral (view cPort cas))
          . C.setKeyspace (view cKeyspace cas)
          . C.setProtocolVersion C.V4
          $ C.defSettings

waitForTaskToComplete :: forall a m. (ES.MonadBH m, MonadIO m, MonadThrow m, FromJSON a) => Int -> ES.TaskNodeId -> m ()
waitForTaskToComplete timeoutSeconds taskNodeId = do
  -- Delay is 0.1 seconds, so retries are limited to timeoutSeconds * 10
  let policy = constantDelay 100000 <> limitRetries (timeoutSeconds * 10)
  let retryCondition _ = fmap not . isTaskComplete
  taskEither <- retrying policy retryCondition (const $ ES.getTask @m @a taskNodeId)
  task <- either errTaskGet pure taskEither
  unless (ES.taskResponseCompleted task) $ do
    throwM $ ReindexFromAnotherIndexError $ "Timed out waiting for task: " <> show taskNodeId
  when (isJust $ ES.taskResponseError task) $ do
    throwM $
      ReindexFromAnotherIndexError $
        "Task failed with error: "
          <> LensBS.unpackLazy8 (Aeson.encode $ ES.taskResponseError task)
  where
    isTaskComplete :: Either ES.EsError (ES.TaskResponse a) -> m Bool
    isTaskComplete (Left e) = throwM $ ReindexFromAnotherIndexError $ "Error response while getting task: " <> show e
    isTaskComplete (Right taskRes) = pure $ ES.taskResponseCompleted taskRes
    --
    errTaskGet :: MonadThrow m => ES.EsError -> m x
    errTaskGet e = throwM $ ReindexFromAnotherIndexError $ "Error response while getting task: " <> show e

newtype ReindexFromAnotherIndexError = ReindexFromAnotherIndexError String
  deriving (Show)

instance Exception ReindexFromAnotherIndexError

--------------------------------------------------------------------------------
-- ReindexIO command monad

newtype ReindexIO a = ReindexIO (ReaderT C.ClientState IndexIO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader C.ClientState,
      MonadThrow,
      MonadCatch
    )

runReindexIO :: IndexEnv -> C.ClientState -> ReindexIO a -> IO a
runReindexIO ixe cas (ReindexIO ma) = runIndexIO ixe (runReaderT ma cas)

instance MonadIndexIO ReindexIO where
  liftIndexIO = ReindexIO . ReaderT . const

instance C.MonadClient ReindexIO where
  liftClient ma = ask >>= \e -> C.runClient e ma
  localState = local

instance MonadLogger ReindexIO where
  log lvl msg = do
    l <- ReindexIO . lift $ asks idxLogger
    Log.log l lvl msg
