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

module Test.CargoHold.S3Test (tests) where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWS
import qualified Amazonka.S3 as S3
import qualified CargoHold.AWS as CargoHoldAWS
import CargoHold.App (Env (..), runAppT)
import CargoHold.Options (Opts (..))
import CargoHold.S3 (AssetAuditLogMetadata (..), getAmzAuditLogMetadata, setAmzAuditLogMetadata, uploadV3)
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Type as MIME
import Conduit (yield)
import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain (..))
import Data.Id
import Data.Qualified (toLocalUnsafe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.UUID as UUID
import Data.Yaml (decodeFileThrow)
import HTTP2.Client.Manager (http2ManagerWithSSLCtx)
import Hasql.Pool.Extended (initPostgresPool)
import Imports
import Network.HTTP.Client (defaultManagerSettings, managerModifyRequest, newManager)
import qualified OpenSSL.Session as SSL
import qualified System.Logger as Logger
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Util.Options (AWSEndpoint (..))

tests :: TestTree
tests =
  testGroup
    "CargoHold.S3"
    [ QC.testProperty
        "audit-log metadata header is ASCII for filenames with umlauts"
        propAuditLogMetadataHeaderIsAscii,
      QC.testProperty
        "audit-log metadata percent-encode/decode roundtrips for non-ASCII filenames"
        propAuditLogMetadataPercentRoundtrip,
      QC.testProperty
        "legacy raw audit-log metadata preserves percent escapes"
        propLegacyRawAuditLogMetadataPreservesPercentEscapes,
      testCase
        "uploadV3 does not swallow asynchronous exceptions"
        testUploadV3AsyncException
    ]

-- | An S3 request that never returns is cancelled by 'timeout' (an asynchronous exception).
-- The upload must not catch it: then 'timeout' returns 'Nothing'.
testUploadV3AsyncException :: Assertion
testUploadV3AsyncException = do
  lgr <- Logger.new (Logger.setLogLevel Logger.Fatal Logger.defSettings)
  -- requests hang before they are sent, so the endpoint is never contacted
  mgr <- newManager defaultManagerSettings {managerModifyRequest = \_ -> forever (threadDelay maxBound)}
  -- 'Env' is strict: the fields 'uploadV3' does not use need inert values (no connections)
  opts :: Opts <- decodeFileThrow "cargohold.integration.yaml"
  http2Mgr <- http2ManagerWithSSLCtx =<< SSL.context
  pool <- initPostgresPool opts.postgresqlPool opts.postgresql Nothing
  amazonkaEnv <-
    AWS.newEnv (pure . AWS.fromKeys (AWS.AccessKey "placeholder") (AWS.SecretKey "placeholder"))
      <&> AWS.configureService (AWS.setEndpoint False "localhost" 1 S3.defaultService)
  let endpoint = AWSEndpoint {_awsHost = "localhost", _awsSecure = False, _awsPort = 1}
      awsEnv =
        CargoHoldAWS.Env
          { logger = lgr,
            s3Bucket = "bucket",
            amazonkaEnv = amazonkaEnv {AWS.manager = mgr},
            amazonkaDownloadEndpoint = endpoint,
            cloudFront = Nothing
          }
      env =
        Env
          { aws = awsEnv,
            appLogger = lgr,
            httpManager = mgr,
            http2Manager = http2Mgr,
            requestId = RequestId "test",
            options = opts,
            localUnit = toLocalUnsafe (Domain "example.com") (),
            multiIngress = mempty,
            hasqlPool = pool
          }
      key = V3.AssetKeyV3 (Id UUID.nil) V3.AssetEternal
      prc = V3.UserPrincipal (Id UUID.nil)
      hdrs = V3.AssetHeaders (MIME.Type (MIME.Application "octet-stream") []) 1
      upload = uploadV3 prc key hdrs Nothing Nothing (yield "x")
  result <- timeout 100000 (runAppT env (runExceptT upload))
  assertBool ("upload finished instead of being cancelled: " <> show result) (isNothing result)

propAuditLogMetadataPercentRoundtrip :: AssetAuditLogMetadata -> QC.Property
propAuditLogMetadataPercentRoundtrip metadata =
  let meta' = metadata {filename = "Mönchsjochhütte"}
      (k, v) = setAmzAuditLogMetadata meta'
   in getAmzAuditLogMetadata [(k, v)] QC.=== Just meta'

propAuditLogMetadataHeaderIsAscii :: AssetAuditLogMetadata -> QC.Property
propAuditLogMetadataHeaderIsAscii metadata =
  let (_, headerValue) =
        setAmzAuditLogMetadata metadata {filename = "Mönchsjochhütte"}
   in QC.counterexample ("non-ASCII S3 metadata header: " <> show headerValue) $
        BS.all (< 128) (encodeUtf8 headerValue)

propLegacyRawAuditLogMetadataPreservesPercentEscapes :: AssetAuditLogMetadata -> QC.Property
propLegacyRawAuditLogMetadataPreservesPercentEscapes metadata =
  let expected = metadata {filename = "%2F and %20 stay unchanged"}
      rawJSON = decodeUtf8 (LBS.toStrict (A.encode expected))
   in getAmzAuditLogMetadata [("wire-metadata", rawJSON)] QC.=== Just expected
