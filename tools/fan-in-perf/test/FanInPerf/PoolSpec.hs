module FanInPerf.PoolSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Misc (Duration (..))
import Hasql.Pool.Extended
import Imports
import PostgresqlConnectionString qualified
import Prometheus qualified as P
import Test.Hspec

spec :: Spec
spec = describe "initPostgresPoolSharingMetrics" $
  it "does not register the pool metrics a second time" $ do
    connStr <- either (const (fail "connection string")) pure (PostgresqlConnectionString.parse "postgresql://u:p@localhost:1/db")
    let cfg = PoolConfig {size = 1, acquisitionTimeout = Duration 1, idlenessTimeout = Duration 1}
    first <- initPostgresPoolFromConnString cfg connStr Nothing
    _ <- initPostgresPoolSharingMetrics first cfg connStr Nothing
    out <- LBS8.unpack <$> P.exportMetricsAsText
    length (filter (isPrefixOf "# TYPE wire_hasql_pool_in_use ") (lines out)) `shouldBe` 1
