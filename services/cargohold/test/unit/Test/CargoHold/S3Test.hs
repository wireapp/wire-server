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

import CargoHold.S3 (AssetAuditLogMetadata (..), getAmzAuditLogMetadata, setAmzAuditLogMetadata)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck as QC

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
        propLegacyRawAuditLogMetadataPreservesPercentEscapes
    ]

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
