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

module Index.Create where

import API.Search.Util (mkBHEnv)
import Brig.App (initHttpManagerWithTLSConfig)
import Brig.Index.Eval qualified as IndexEval
import Brig.Index.Options
import Brig.Index.Options qualified as IndexOpts
import Brig.Options (Opts (galley))
import Brig.Options qualified as BrigOpts
import Control.Lens ((.~))
import Control.Lens.Combinators (none)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Bloodhound qualified as ES
import Imports
import System.Logger.Class qualified as Log
import System.Random as Random
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString

-- FUTUREWORK: Make Bloodhound capable of getting a mapping and add test here to make sure
-- the intended mapping is set on the created index. Also add a test to ensure when the index
-- already exists, the existing mapping is not updated.
spec :: BrigOpts.Opts -> IO TestTree
spec brigOpts =
  pure $
    testGroup
      "brig-index create"
      [ testCase "should create an index when it is not present" $ testCreateIndexWhenNotPresent brigOpts,
        testCase "should not update anything when index already exists" $ testCreateIndexWhenPresent brigOpts
      ]

testCreateIndexWhenNotPresent :: BrigOpts.Opts -> Assertion
testCreateIndexWhenNotPresent brigOpts = do
  let (ES.Server esURL) = brigOpts.elasticsearch.url
  case parseURI strictURIParserOptions (Text.encodeUtf8 esURL) of
    Left e -> fail $ "Invalid ES URL: " <> show esURL <> "\nerror: " <> show e
    Right esURI -> do
      indexName <- ES.IndexName . Text.pack <$> replicateM 20 (Random.randomRIO ('a', 'z'))
      let replicas = 2
          shards = 2
          refreshInterval = 5
      let connSettings =
            ESConnectionSettings
              { esServer = esURI,
                esIndex = indexName,
                esCaCert = brigOpts.elasticsearch.caCert,
                esInsecureSkipVerifyTls = brigOpts.elasticsearch.insecureSkipVerifyTls,
                esCredentials = brigOpts.elasticsearch.credentials
              }
      let esSettings =
            IndexOpts.localElasticSettings
              & IndexOpts.esConnection .~ connSettings
              & IndexOpts.esIndexReplicas .~ ES.ReplicaCount replicas
              & IndexOpts.esIndexShardCount .~ shards
              & IndexOpts.esIndexRefreshInterval .~ refreshInterval
      devNullLogger <- Log.create (Log.Path "/dev/null")
      IndexEval.runCommand devNullLogger (IndexOpts.Create esSettings (galley brigOpts))
      mgr <- liftIO $ initHttpManagerWithTLSConfig connSettings.esInsecureSkipVerifyTls connSettings.esCaCert
      let bEnv = (mkBHEnv esURL mgr) {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername "elastic") (ES.EsPassword "changeme")}
      ES.runBH bEnv $ do
        indexExists <- ES.indexExists indexName
        lift $
          assertBool "Index should exist" indexExists
        eitherIndexSettings <- ES.getIndexSettings indexName
        lift $ do
          case eitherIndexSettings of
            Left err -> fail $ "Failed to fetch index settings with error: " <> show err
            Right indexSettings -> do
              assertEqual "Shard count should be set" (ES.ShardCount replicas) (ES.indexShards . ES.sSummaryFixedSettings $ indexSettings)
              assertEqual "Replica count should be set" (ES.ReplicaCount replicas) (ES.indexReplicas . ES.sSummaryFixedSettings $ indexSettings)
              -- Check if the `RefreshInterval` is part of `UpdateIndexSettings`.
              -- There can be more settings. E.g. ElasticSearch 7 has these:
              -- `[RefreshInterval 5s, RoutingAllocationInclude (NodeAttrFilter {nodeAttrFilterName = NodeAttrName "_tier_preference", nodeAttrFilterValues = "data_content" :| []} :| [])]`
              assertBool "Refresh interval should be set" $ (ES.RefreshInterval refreshInterval) `elem` (ES.sSummaryUpdateable indexSettings)

-- assertEqual "Refresh interval should be set" [ES.RefreshInterval refreshInterval] (ES.sSummaryUpdateable indexSettings)

testCreateIndexWhenPresent :: BrigOpts.Opts -> Assertion
testCreateIndexWhenPresent brigOpts = do
  let (ES.Server esURL) = brigOpts.elasticsearch.url
  case parseURI strictURIParserOptions (Text.encodeUtf8 esURL) of
    Left e -> fail $ "Invalid ES URL: " <> show esURL <> "\nerror: " <> show e
    Right esURI -> do
      indexName <- ES.IndexName . Text.pack <$> replicateM 20 (Random.randomRIO ('a', 'z'))
      let replicas = 2
          shards = 2
          refreshInterval = 5
          connSettings =
            ESConnectionSettings
              { esServer = esURI,
                esIndex = indexName,
                esCaCert = brigOpts.elasticsearch.caCert,
                esInsecureSkipVerifyTls = brigOpts.elasticsearch.insecureSkipVerifyTls,
                esCredentials = brigOpts.elasticsearch.credentials
              }
          esSettings =
            IndexOpts.localElasticSettings
              & IndexOpts.esConnection .~ connSettings
              & IndexOpts.esIndexReplicas .~ ES.ReplicaCount replicas
              & IndexOpts.esIndexShardCount .~ shards
              & IndexOpts.esIndexRefreshInterval .~ refreshInterval
      mgr <- liftIO $ initHttpManagerWithTLSConfig connSettings.esInsecureSkipVerifyTls connSettings.esCaCert
      let bEnv = (mkBHEnv esURL mgr) {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername "elastic") (ES.EsPassword "changeme")}
      ES.runBH bEnv $ do
        _ <- ES.createIndex (ES.IndexSettings (ES.ShardCount 1) (ES.ReplicaCount 1)) indexName
        indexExists <- ES.indexExists indexName
        lift $
          assertBool "Index should exist" indexExists
      devNullLogger <- Log.create (Log.Path "/dev/null")
      IndexEval.runCommand devNullLogger (IndexOpts.Create esSettings (galley brigOpts))
      ES.runBH bEnv $ do
        indexExists <- ES.indexExists indexName
        lift $
          assertBool "Index should still exist" indexExists
        eitherIndexSettings <- ES.getIndexSettings indexName
        lift $ do
          case eitherIndexSettings of
            Left err -> fail $ "Failed to fetch index settings with error: " <> show err
            Right indexSettings -> do
              assertEqual "Shard count should not be updated" (ES.ShardCount 1) (ES.indexShards . ES.sSummaryFixedSettings $ indexSettings)
              assertEqual "Replica count should not be updated" (ES.ReplicaCount 1) (ES.indexReplicas . ES.sSummaryFixedSettings $ indexSettings)
              -- Ensure that the `RefreshInterval` is not part of `UpdateIndexSettings`.
              -- There can be more settings. E.g. ElasticSearch 7 has this by default:
              -- `[RoutingAllocationInclude (NodeAttrFilter {nodeAttrFilterName = NodeAttrName "_tier_preference", nodeAttrFilterValues = "data_content" :| []} :| [])]`
              assertBool "Refresh interval should not be updated" $
                none
                  ( \case
                      ES.RefreshInterval _ -> True
                      _otherwise -> False
                  )
                  (ES.sSummaryUpdateable indexSettings)
