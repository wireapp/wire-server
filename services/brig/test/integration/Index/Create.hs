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

module Index.Create where

import qualified Brig.Index.Eval as IndexEval
import qualified Brig.Index.Options as IndexOpts
import qualified Brig.Options as BrigOpts
import Control.Lens ((.~), (^.))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.Bloodhound as ES
import Imports
import qualified Network.HTTP.Client as HTTP
import qualified System.Logger.Class as Log
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
  let esURL = brigOpts ^. BrigOpts.elasticsearchL . BrigOpts.urlL
  case parseURI strictURIParserOptions (Text.encodeUtf8 esURL) of
    Left e -> fail $ "Invalid ES URL: " <> show esURL <> "\nerror: " <> show e
    Right esURI -> do
      indexName <- ES.IndexName . Text.pack <$> (replicateM 20 $ Random.randomRIO ('a', 'z'))
      let replicas = 2
          shards = 2
          refreshInterval = 5
      let esSettings =
            IndexOpts.localElasticSettings
              & IndexOpts.esServer .~ esURI
              & IndexOpts.esIndex .~ indexName
              & IndexOpts.esIndexReplicas .~ (ES.ReplicaCount replicas)
              & IndexOpts.esIndexShardCount .~ shards
              & IndexOpts.esIndexRefreshInterval .~ refreshInterval
      devNullLogger <- Log.create (Log.Path "/dev/null")
      IndexEval.runCommand devNullLogger (IndexOpts.Create esSettings)
      ES.withBH HTTP.defaultManagerSettings (ES.Server esURL) $ do
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
              assertEqual "Refresh internval should be set" [ES.RefreshInterval refreshInterval] (ES.sSummaryUpdateable indexSettings)

testCreateIndexWhenPresent :: BrigOpts.Opts -> Assertion
testCreateIndexWhenPresent brigOpts = do
  let esURL = brigOpts ^. BrigOpts.elasticsearchL . BrigOpts.urlL
  case parseURI strictURIParserOptions (Text.encodeUtf8 esURL) of
    Left e -> fail $ "Invalid ES URL: " <> show esURL <> "\nerror: " <> show e
    Right esURI -> do
      indexName <- ES.IndexName . Text.pack <$> (replicateM 20 $ Random.randomRIO ('a', 'z'))
      ES.withBH HTTP.defaultManagerSettings (ES.Server esURL) $ do
        _ <- ES.createIndex (ES.IndexSettings (ES.ShardCount 1) (ES.ReplicaCount 1)) indexName
        indexExists <- ES.indexExists indexName
        lift $
          assertBool "Index should exist" indexExists
      let replicas = 2
          shards = 2
          refreshInterval = 5
      let esSettings =
            IndexOpts.localElasticSettings
              & IndexOpts.esServer .~ esURI
              & IndexOpts.esIndex .~ indexName
              & IndexOpts.esIndexReplicas .~ (ES.ReplicaCount replicas)
              & IndexOpts.esIndexShardCount .~ shards
              & IndexOpts.esIndexRefreshInterval .~ refreshInterval
      devNullLogger <- Log.create (Log.Path "/dev/null")
      IndexEval.runCommand devNullLogger (IndexOpts.Create esSettings)
      ES.withBH HTTP.defaultManagerSettings (ES.Server esURL) $ do
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
              assertEqual "Refresh internval should not be updated" [] (ES.sSummaryUpdateable indexSettings)
