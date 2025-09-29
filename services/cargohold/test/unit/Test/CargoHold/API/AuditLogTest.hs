-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.CargoHold.API.AuditLogTest (tests) where

import CargoHold.API.AuditLog
import CargoHold.S3 (AssetAuditLogMetadata (..), S3AssetMeta (..))
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Type as MIME
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Domain (Domain (..), domainText)
import Data.Id (UserId, botUserId)
import Data.Qualified
import qualified Data.Text as T
import Imports
import System.Logger.Extended (LoggerT, runWithLogger)
import Test.CargoHold.API.LogJSON
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import URI.ByteString
import qualified Wire.API.Asset as Asset
import qualified Wire.Arbitrary ()

tests :: TestTree
tests =
  testGroup
    "CargoHold.API.AuditLog"
    [ QC.testProperty "logUpload matches expected JSON" propLogUpload,
      QC.testProperty "logDownload with meta matches expected JSON" propLogDownload,
      QC.testProperty "logSignedURLCreation (maybe meta) matches expected JSON" propLogSignedURLCreation,
      QC.testProperty "logDownloadRemoteAsset matches expected JSON" propLogDownloadRemoteAsset
    ]

propLogUpload :: Domain -> V3.Principal -> AssetAuditLogMetadata -> T.Text -> QC.Property
propLogUpload dom princ meta pathTxt = QC.ioProperty $ do
  let lprin = toLocalUnsafe dom princ
  (_, logs) <- withStructuredJSONLogger $ \logger ->
    runWithLogger logger (logUpload lprin (Just meta) pathTxt :: LoggerT IO ())
  pure $ case logs of
    (obj : _) ->
      let expected =
            Aeson.object
              [ "level" .= ("Info" :: T.Text),
                "msgs" .= ["Asset audit log: upload" :: T.Text],
                "audit" .= ("True" :: T.Text),
                "event" .= ("file-upload" :: T.Text),
                "uploader.type" .= uploaderType princ,
                "uploader.id" .= uploaderId princ,
                "uploader.domain" .= domainText dom,
                "conversation.id" .= T.pack (show (qUnqualified meta.convId)),
                "conversation.domain" .= domainText (qDomain meta.convId),
                "file.name" .= meta.filename,
                "file.type" .= MIME.showType (Asset.unAssetMIMEType meta.filetype),
                "upload.path" .= pathTxt
              ]
       in obj QC.=== expected
    _ -> counterexample "No logs emitted" False

propLogDownload :: Domain -> V3.Principal -> V3.Principal -> AssetAuditLogMetadata -> T.Text -> QC.Property
propLogDownload dom owner downloader meta pathTxt = QC.ioProperty $ do
  let qDownloader = Qualified downloader dom
      s3meta =
        S3AssetMeta
          { v3AssetOwner = owner,
            v3AssetToken = Nothing,
            v3AssetType = MIME.Type (MIME.Application "octet-stream") [],
            v3AssetAuditLogMetadata = Just meta
          }
  (_, logs) <- withStructuredJSONLogger $ \logger ->
    runWithLogger logger (logDownload (Just qDownloader) s3meta pathTxt :: LoggerT IO ())
  pure $ case logs of
    (obj : _) ->
      let expected =
            Aeson.object
              [ "level" .= ("Info" :: T.Text),
                "msgs" .= ["Asset audit log: download" :: T.Text],
                "audit" .= ("True" :: T.Text),
                "event" .= ("file-download" :: T.Text),
                "downloader.type" .= uploaderType downloader,
                "downloader.id" .= uploaderId downloader,
                "downloader.domain" .= domainText dom,
                "conversation.id" .= T.pack (show (qUnqualified meta.convId)),
                "conversation.domain" .= domainText (qDomain meta.convId),
                "file.name" .= meta.filename,
                "file.type" .= MIME.showType (Asset.unAssetMIMEType meta.filetype),
                "download.path" .= pathTxt
              ]
       in obj QC.=== expected
    _ -> counterexample "No logs emitted" False

propLogSignedURLCreation :: Qualified V3.Principal -> Maybe AssetAuditLogMetadata -> QC.Property
propLogSignedURLCreation qDownloader mMeta = QC.ioProperty $ do
  let uriEither = parseURI strictURIParserOptions "https://example.test/path/ok"
      uri = either (error . show) id uriEither
      mS3Meta =
        ( \meta ->
            S3AssetMeta
              { v3AssetOwner = qUnqualified qDownloader,
                v3AssetToken = Nothing,
                v3AssetType = MIME.Type (MIME.Application "octet-stream") [],
                v3AssetAuditLogMetadata = Just meta
              }
        )
          <$> mMeta
  (_, logs) <- withStructuredJSONLogger $ \logger ->
    runWithLogger logger (logSignedURLCreation (Just qDownloader) mS3Meta uri :: LoggerT IO ())
  pure $ case logs of
    (obj : _) ->
      let (cid, cdom, fname, ftype) = case mMeta of
            Just meta ->
              ( T.pack (show (qUnqualified meta.convId)),
                domainText (qDomain meta.convId),
                meta.filename,
                MIME.showType (Asset.unAssetMIMEType meta.filetype)
              )
            Nothing -> ("N/A", "N/A", "N/A", "N/A")
          expected =
            Aeson.object
              [ "level" .= ("Info" :: T.Text),
                "msgs" .= ["Asset audit log: signed URL creation" :: T.Text],
                "audit" .= ("True" :: T.Text),
                "event" .= ("download-url-creation" :: T.Text),
                "downloader.type" .= uploaderType (qUnqualified qDownloader),
                "downloader.id" .= uploaderId (qUnqualified qDownloader),
                "downloader.domain" .= domainText (qDomain qDownloader),
                "download.url.host" .= ("example.test" :: T.Text),
                "download.url.path" .= ("/path/ok" :: T.Text),
                "file.name" .= fname,
                "file.type" .= ftype,
                "conversation.id" .= cid,
                "conversation.domain" .= cdom
              ]
       in obj QC.=== expected
    _ -> counterexample "No logs emitted" False

propLogDownloadRemoteAsset :: Domain -> UserId -> Domain -> QC.Property
propLogDownloadRemoteAsset domLocal uid domRemote = QC.ioProperty $ do
  let luid = toLocalUnsafe domLocal uid
      rmt = toRemoteUnsafe domRemote ()
  (_, logs) <- withStructuredJSONLogger $ \logger ->
    runWithLogger logger (logDownloadRemoteAsset luid rmt :: LoggerT IO ())
  pure $ case logs of
    (obj : _) ->
      let expected =
            Aeson.object
              [ "level" .= ("Info" :: T.Text),
                "msgs" .= ["Asset audit log: remote download" :: T.Text],
                "audit" .= ("True" :: T.Text),
                "event" .= ("file-download" :: T.Text),
                "downloader.type" .= ("user" :: T.Text),
                "downloader.id" .= T.pack (show uid),
                "downloader.domain" .= domainText domLocal,
                "remote.domain" .= domainText domRemote
              ]
       in obj QC.=== expected
    _ -> counterexample "No logs emitted" False

uploaderType :: V3.Principal -> T.Text
uploaderType = \case
  V3.UserPrincipal {} -> "user" :: T.Text
  V3.BotPrincipal {} -> "bot"
  V3.ProviderPrincipal {} -> "provider"

uploaderId :: V3.Principal -> T.Text
uploaderId = \case
  V3.UserPrincipal u -> T.pack (show u)
  V3.BotPrincipal b -> T.pack (show (botUserId b))
  V3.ProviderPrincipal p -> T.pack (show p)
