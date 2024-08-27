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

module Brig.Index.Eval
  ( runCommand,
  )
where

import Brig.App (initHttpManagerWithTLSConfig, mkIndexEnv)
import Brig.Index.Options
import Brig.Options
import Brig.User.Search.Index
import Cassandra (Client, runClient)
import Cassandra.Options
import Cassandra.Util (defInitCassandra)
import Control.Exception (throwIO)
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.UTF8 qualified as UTF8
import Data.Credentials (Credentials (..))
import Data.Id
import Database.Bloodhound qualified as ES
import Database.Bloodhound.Internal.Client (BHEnv (..))
import Imports
import Polysemy
import Polysemy.Embed (runEmbedded)
import Polysemy.Error
import Polysemy.TinyLog hiding (Logger)
import System.Logger qualified as Log
import System.Logger.Class (Logger)
import Util.Options (initCredentials)
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.BlockListStore (BlockListStore)
import Wire.BlockListStore.Cassandra
import Wire.FederationAPIAccess
import Wire.FederationAPIAccess.Interpreter (noFederationAPIAccess)
import Wire.FederationConfigStore (FederationConfigStore)
import Wire.FederationConfigStore.Cassandra (interpretFederationDomainConfig)
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess.Rpc
import Wire.IndexedUserStore
import Wire.IndexedUserStore.Bulk (IndexedUserStoreBulk)
import Wire.IndexedUserStore.Bulk qualified as IndexedUserStoreBulk
import Wire.IndexedUserStore.Bulk.ElasticSearch (interpretIndexedUserStoreBulk)
import Wire.IndexedUserStore.ElasticSearch
import Wire.IndexedUserStore.MigrationStore (IndexedUserMigrationStore)
import Wire.IndexedUserStore.MigrationStore.ElasticSearch
import Wire.ParseException
import Wire.Rpc
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Logger.TinyLog
import Wire.Sem.Metrics
import Wire.Sem.Metrics.IO
import Wire.UserKeyStore (UserKeyStore)
import Wire.UserKeyStore.Cassandra
import Wire.UserSearch.Migration (MigrationException)
import Wire.UserStore
import Wire.UserStore.Cassandra
import Wire.UserSubsystem.Error

type BrigIndexEffectStack =
  [ IndexedUserStoreBulk,
    UserKeyStore,
    BlockListStore,
    Error UserSubsystemError,
    FederationAPIAccess FederatorClient,
    Error FederationError,
    UserStore,
    IndexedUserStore,
    Error IndexedUserStoreError,
    IndexedUserMigrationStore,
    Error MigrationException,
    FederationConfigStore,
    GalleyAPIAccess,
    Error ParseException,
    Rpc,
    Metrics,
    TinyLog,
    Concurrency 'Unsafe,
    Embed IO,
    Final IO
  ]

runSem :: ESConnectionSettings -> CassandraSettings -> Endpoint -> Logger -> Sem BrigIndexEffectStack a -> IO a
runSem esConn cas galleyEndpoint logger action = do
  mgr <- initHttpManagerWithTLSConfig esConn.esInsecureSkipVerifyTls esConn.esCaCert
  mEsCreds :: Maybe Credentials <- for esConn.esCredentials initCredentials
  casClient <- defInitCassandra (toCassandraOpts cas) logger
  let bhEnv =
        BHEnv
          { bhServer = toESServer esConn.esServer,
            bhManager = mgr,
            bhRequestHook = maybe pure (\creds -> ES.basicAuthHook (ES.EsUsername creds.username) (ES.EsPassword creds.password)) mEsCreds
          }
      indexedUserStoreConfig =
        IndexedUserStoreConfig
          { conn =
              ESConn
                { indexName = esConn.esIndex,
                  env = bhEnv
                },
            additionalConn = Nothing
          }
      reqId = (RequestId "brig-index")
  runFinal
    . embedToFinal
    . unsafelyPerformConcurrency
    . loggerToTinyLogReqId reqId logger
    . ignoreMetrics
    . runRpcWithHttp mgr reqId
    . throwErrorToIOFinal @ParseException
    . interpretGalleyAPIAccessToRpc mempty galleyEndpoint
    . runEmbedded (runClient casClient)
    . interpretFederationDomainConfig Nothing mempty
    . raiseUnder @(Embed Client)
    . throwErrorToIOFinal @MigrationException
    . interpretIndexedUserMigrationStoreES bhEnv
    . throwErrorToIOFinal @IndexedUserStoreError
    . interpretIndexedUserStoreES indexedUserStoreConfig
    . interpretUserStoreCassandra casClient
    . throwErrorToIOFinal @FederationError
    . noFederationAPIAccess
    . throwErrorToIOFinal @UserSubsystemError
    . interpretBlockListStoreToCassandra casClient
    . interpretUserKeyStoreCassandra casClient
    . interpretIndexedUserStoreBulk
    $ action

throwErrorToIOFinal :: (Exception e, Member (Final IO) r) => InterpreterFor (Error e) r
throwErrorToIOFinal action = do
  runError action >>= \case
    Left e -> embedFinal $ throwIO e
    Right a -> pure a

runCommand :: Logger -> Command -> IO ()
runCommand l = \case
  Create es galley -> do
    e <- initIndex (es ^. esConnection) galley
    runIndexIO e $ createIndexIfNotPresent (mkCreateIndexSettings es)
  Reset es galley -> do
    e <- initIndex (es ^. esConnection) galley
    runIndexIO e $ resetIndex (mkCreateIndexSettings es)
  Reindex es cas galley -> do
    runSem (es ^. esConnection) cas galley l $
      IndexedUserStoreBulk.syncAllUsers
  ReindexSameOrNewer es cas galley -> do
    runSem (es ^. esConnection) cas galley l $
      IndexedUserStoreBulk.forceSyncAllUsers
  UpdateMapping esConn galley -> do
    e <- initIndex esConn galley
    runIndexIO e updateMapping
  Migrate es cas galley -> do
    runSem (es ^. esConnection) cas galley l $
      IndexedUserStoreBulk.migrateData
  ReindexFromAnotherIndex reindexSettings -> do
    mgr <-
      initHttpManagerWithTLSConfig
        (reindexSettings ^. reindexEsConnection . to esInsecureSkipVerifyTls)
        (reindexSettings ^. reindexEsConnection . to esCaCert)
    mCreds <- for (reindexSettings ^. reindexEsConnection . to esCredentials) initCredentials
    let bhEnv = initES (reindexSettings ^. reindexEsConnection . to esServer) mgr mCreds
    ES.runBH bhEnv $ do
      let src = reindexSettings ^. reindexEsConnection . to esIndex
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
        Left e -> throwM $ ReindexFromAnotherIndexError $ "Error occurred while running reindex: " <> show e
        Right taskNodeId -> do
          Log.info l $ Log.field "taskNodeId" (show taskNodeId)
          waitForTaskToComplete @ES.ReindexResponse timeoutSeconds taskNodeId
          Log.info l $ Log.msg ("Finished reindexing" :: ByteString)
  where
    initIndex :: ESConnectionSettings -> Endpoint -> IO IndexEnv
    initIndex esConn gly = do
      mgr <- initHttpManagerWithTLSConfig esConn.esInsecureSkipVerifyTls esConn.esCaCert
      let esOpts =
            ElasticSearchOpts
              { url = toESServer esConn.esServer,
                index = esConn.esIndex,
                credentials = esConn.esCredentials,
                insecureSkipVerifyTls = esConn.esInsecureSkipVerifyTls,
                caCert = esConn.esCaCert,
                additionalWriteIndex = Nothing,
                additionalWriteIndexUrl = Nothing,
                additionalCredentials = Nothing,
                additionalInsecureSkipVerifyTls = False,
                additionalCaCert = Nothing
              }

      mkIndexEnv esOpts l gly mgr

    initES esURI mgr mCreds =
      let env = ES.mkBHEnv (toESServer esURI) mgr
       in maybe env (\(creds :: Credentials) -> env {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername creds.username) (ES.EsPassword creds.password)}) mCreds

waitForTaskToComplete :: forall a m. (ES.MonadBH m, MonadThrow m, FromJSON a) => Int -> ES.TaskNodeId -> m ()
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
          <> UTF8.toString (Aeson.encode $ ES.taskResponseError task)
  where
    isTaskComplete :: Either ES.EsError (ES.TaskResponse a) -> m Bool
    isTaskComplete (Left e) = throwM $ ReindexFromAnotherIndexError $ "Error response while getting task: " <> show e
    isTaskComplete (Right taskRes) = pure $ ES.taskResponseCompleted taskRes

    errTaskGet :: ES.EsError -> m x
    errTaskGet e = throwM $ ReindexFromAnotherIndexError $ "Error response while getting task: " <> show e

newtype ReindexFromAnotherIndexError = ReindexFromAnotherIndexError String
  deriving (Show)

instance Exception ReindexFromAnotherIndexError
