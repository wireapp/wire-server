{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module CargoHold.S3
  ( S3AssetKey (..),
    S3AssetMeta (..),
    AssetAuditLogMetadata (..),
    setAmzAuditLogMetadata,
    getAmzAuditLogMetadata,
    uploadV3,
    downloadV3,
    getMetadataV3,
    updateMetadataV3,
    deleteV3,
    uploadBackupBlob,
    deleteObject,
    headObjectExists,
    listCommonPrefixes,
    listObjects,
    deleteAllVersions,
    mkKey,
    signedURL,
    signedBackupURL,
    -- Legacy
    plainKey,
    otrKey,
    getMetadata,
    getOtrMetadata,
  )
where

import Amazonka hiding (Error)
import qualified Amazonka
import Amazonka.S3
import Amazonka.S3.Lens
import qualified Amazonka.S3.StreamingUpload as SU
import CargoHold.API.Error
import CargoHold.AWS (amazonkaEnvWithDownloadEndpoint)
import qualified CargoHold.AWS as AWS
import CargoHold.App hiding (Env)
import CargoHold.Options
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Conduit
import Control.Error (throwE)
import Control.Lens hiding (parts, (.=), (:<), (:>))
import Control.Monad.Catch (try)
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Conduit.Binary
import Data.Domain (Domain, domainText)
import qualified Data.HashMap.Lazy as HML
import Data.Id
import Data.Qualified (Qualified)
import qualified Data.Schema as S
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import qualified Data.UUID as UUID
import Imports
import Network.HTTP.Types.Status (status404)
import qualified Network.HTTP.Types.URI as HTTPURI
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (.=), (~~))
import Test.QuickCheck (Arbitrary (..))
import URI.ByteString
import Wire.API.Asset (AssetMIMEType)
import Wire.BackupBlobStore (BlobUploadError (..))

newtype S3AssetKey = S3AssetKey {s3Key :: Text}
  deriving (Eq, Show, ToByteString)

data AssetAuditLogMetadata = AssetAuditLogMetadata
  { convId :: Qualified ConvId,
    filename :: Text,
    filetype :: AssetMIMEType
  }
  deriving (Eq, Show)
  deriving (A.FromJSON, A.ToJSON) via S.Schema AssetAuditLogMetadata

instance S.ToSchema AssetAuditLogMetadata where
  schema =
    S.object $
      AssetAuditLogMetadata
        <$> convId S..= S.field "convId" S.schema
        <*> filename S..= S.field "filename" S.schema
        <*> filetype S..= S.field "filetype" S.schema

-- | Asset metadata tracked in S3.
data S3AssetMeta = S3AssetMeta
  { v3AssetOwner :: V3.Principal,
    v3AssetToken :: Maybe V3.AssetToken,
    v3AssetType :: MIME.Type, -- should be ignored, see note on overrideMimeTypeAsOctetStream. FUTUREWORK: remove entirely.
    v3AssetAuditLogMetadata :: Maybe AssetAuditLogMetadata
  }
  deriving (Show)

-- Note [overrideMimeTypeAsOctetStream]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The asset V3 upload API allows setting arbitrary Asset MIME types on the
-- "outside" of an uploaded (generally encrypted, exception: public profile
-- pictures) asset. (outside meaning outside the encrypted blob in the second
-- part of the multipart/mixed body section).  However, outside-MIME types are
-- not really used by Wire clients. To avoid any potential abuse of setting
-- unexpected outside MIME types, yet remain backwards-compatible with older
-- clients still setting mime types different to application/octet-stream, we
-- ignore the uploaded mimetype header and force it to be
-- application/octet-stream.

uploadV3 ::
  V3.Principal ->
  V3.AssetKey ->
  V3.AssetHeaders ->
  Maybe AssetAuditLogMetadata ->
  Maybe V3.AssetToken ->
  -- | streaming payload
  ConduitM () ByteString (ResourceT IO) () ->
  Handler ()
uploadV3 prc (s3Key . mkKey -> key) (V3.AssetHeaders _ cl) mAuditLogMeta tok src = do
  let logCtx =
        "remote" .= val "S3"
          ~~ "asset.owner" .= toByteString prc
          ~~ "asset.key" .= key
          ~~ "asset.type" .= MIME.showType ct
          ~~ "asset.size" .= cl
  Log.info $ logCtx ~~ msg (val "Uploading asset")
  awsEnv <- asks (.aws)
  let createReq =
        newCreateMultipartUpload (BucketName awsEnv.s3Bucket) (ObjectKey key)
          & createMultipartUpload_contentType ?~ MIME.showType ct
          & createMultipartUpload_metadata .~ metaHeaders tok prc mAuditLogMeta
  complete <- streamUpload logCtx createReq (fromIntegral cl) src
  unless (isJust complete) $ throwE incompleteBody
  where
    ct = octets -- see note “overrideMimeTypeAsOctetStream”

-- | Upload a backup file. Unlike 'uploadV3', no metadata is stored. 'BlobLengthMismatch' if the
-- stream length differs from the declared one; the S3 object may exist then. Returns the
-- @x-amz-version-id@ of the new object as answered by S3 ('Nothing' on unversioned buckets).
uploadBackupBlob ::
  S3AssetKey ->
  Word64 ->
  ConduitM () ByteString (ResourceT IO) () ->
  Handler (Either BlobUploadError (Maybe Text))
uploadBackupBlob (S3AssetKey key) len src = do
  let logCtx = "remote" .= val "S3" ~~ "backup.key" .= key ~~ "backup.size" .= len
  Log.info $ logCtx ~~ msg (val "Uploading backup file")
  awsEnv <- asks (.aws)
  let createReq =
        newCreateMultipartUpload (BucketName awsEnv.s3Bucket) (ObjectKey key)
          & createMultipartUpload_contentType ?~ MIME.showType octets
  complete <- streamUpload logCtx createReq len src
  pure $ case complete of
    Nothing -> Left BlobLengthMismatch
    Just r -> Right (fromObjectVersionId <$> r ^. completeMultipartUploadResponse_versionId)

-- | Stream @src@ to S3 as multipart upload. 'Nothing' if the stream length differs from @len@.
streamUpload ::
  (Log.Msg -> Log.Msg) ->
  CreateMultipartUpload ->
  Word64 ->
  ConduitM () ByteString (ResourceT IO) () ->
  Handler (Maybe CompleteMultipartUploadResponse)
streamUpload logCtx createReq len src = do
  awsEnv <- asks (.aws)
  cntRef <- liftIO $ newIORef (0 :: Word64)

  -- byte counter for validating that an upload was completely uploaded from the client
  let countC = awaitForever $ \bs -> do
        liftIO $ modifyIORef' cntRef (+ fromIntegral (BS.length bs))
        yield bs

  result <-
    liftIO . try . runResourceT . runConduit $
      src
        .| chunksOfCE (fromIntegral defaultChunkSize)
        .| isolate (fromIntegral len)
        .| countC
        .| SU.streamUpload awsEnv.amazonkaEnv Nothing createReq

  case result of
    Left (e :: SomeException) -> do
      Log.err $ logCtx ~~ msg ("Unhandled error while uploading: " <> show e)
      throwE serverError
    Right uploadResult -> do
      bytesSeen <- liftIO $ readIORef cntRef
      case uploadResult of
        _ | bytesSeen /= len -> pure Nothing
        Right r -> pure (Just r)
        Left ((abortResponse, exception) :: (AbortMultipartUploadResponse, SomeException)) -> do
          Log.err $
            logCtx
              ~~ msg ("Multipart upload failed – aborted: " <> show (abortResponse, exception))
          throwE serverError

-- | Turn a 'ResourceT IO' action into a pure @Conduit@.
--
-- This is possible because @Conduit@ itself is a monad transformer over
-- 'ResourceT IO'. Removing the outer 'ResourceT IO' layer makes it possible to
-- pass this @Conduit@ to resource-oblivious code.
flattenResourceT ::
  ResourceT IO (ConduitT () ByteString (ResourceT IO) ()) ->
  ConduitT () ByteString (ResourceT IO) ()
flattenResourceT = join . lift

downloadV3 ::
  V3.AssetKey ->
  Handler (ConduitM () ByteString (ResourceT IO) ())
downloadV3 (s3Key . mkKey -> key) = do
  env <- asks (.aws)
  pure . flattenResourceT $ view (getObjectResponse_body . _ResponseBody) <$> AWS.execStream env req
  where
    req :: Text -> GetObject
    req b =
      newGetObject (BucketName b) (ObjectKey key)
        & getObject_responseContentType ?~ MIME.showType octets

getMetadataV3 :: V3.AssetKey -> Handler (Maybe S3AssetMeta)
getMetadataV3 (s3Key . mkKey -> key) = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.key" .= key
      ~~ msg
        (val "Getting asset metadata")
  maybe (pure Nothing) handle =<< execCatch req
  where
    req b = newHeadObject (BucketName b) (ObjectKey key)
    handle r = do
      let ct = fromMaybe octets (MIME.parseMIMEType =<< r ^. headObjectResponse_contentType)
      let meta = HML.toList $ r ^. headObjectResponse_metadata
      pure $ parse ct meta
    parse ct h =
      S3AssetMeta
        <$> getAmzMetaPrincipal h
        <*> Just (getAmzMetaToken h)
        <*> Just ct
        <*> Just (getAmzAuditLogMetadata h)

deleteV3 :: V3.AssetKey -> Handler ()
deleteV3 = flip deleteObject Nothing . mkKey

-- | Deletes the given version, or plain delete if 'Nothing' (on a versioned bucket that only
-- adds a delete marker). Idempotent: S3 does not fail for missing keys.
deleteObject :: S3AssetKey -> Maybe Text -> Handler ()
deleteObject (S3AssetKey key) version = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "key" .= key
      ~~ msg (val "Deleting object")
  void $ exec req
  where
    req b =
      newDeleteObject (BucketName b) (ObjectKey key)
        & deleteObject_versionId .~ (ObjectVersionId <$> version)

updateMetadataV3 :: V3.AssetKey -> S3AssetMeta -> Handler ()
updateMetadataV3 (s3Key . mkKey -> key) meta = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.owner" .= show (v3AssetOwner meta)
      ~~ "asset.key" .= key
      ~~ msg (val "Updating asset metadata")
  void $ exec req
  where
    ct :: MIME.Type
    ct = octets -- See note on overrideMimeTypeAsOctetStream
    copySrc b =
      decodeLatin1 . LBS.toStrict . toLazyByteString $
        urlEncode [] $
          Text.encodeUtf8 (b <> "/" <> key)
    req b =
      newCopyObject (BucketName b) (copySrc b) (ObjectKey key)
        & copyObject_contentType ?~ MIME.showType ct
        & copyObject_metadataDirective ?~ MetadataDirective_REPLACE
        & copyObject_metadata .~ metaHeaders (v3AssetToken meta) (v3AssetOwner meta) meta.v3AssetAuditLogMetadata

-- | Generate an `URI` for asset download redirects
--
-- If @aws.multiIngress@ is configured, the endpoint is looked up from this
-- `Map` with the @Z-Host@ header's value as key. Otherwise (the default case
-- that applies to most deployments), use the default AWS environment; i.e. the
-- environment with @aws.s3DownloadEndpoint@.
signedURL :: (ToByteString p) => p -> Maybe Domain -> Handler URI
signedURL path mbHost = do
  e <- awsEnvForHost
  presignedURL e path
  where
    awsEnvForHost :: Handler AWS.Env
    awsEnvForHost = do
      multiIngressConf <- asks (.multiIngress)
      if null multiIngressConf
        then asks (.aws)
        else awsEnvForHost' mbHost multiIngressConf
      where
        awsEnvForHost' :: Maybe Domain -> Map Domain AWS.Env -> Handler AWS.Env
        awsEnvForHost' Nothing _ = do
          Log.debug $
            msg (val "awsEnvForHost - multiIngress configured, but no Z-Host header provided.")
          throwE noMatchingAssetEndpoint
        awsEnvForHost' (Just host) multiIngressConf = do
          Log.debug $
            "host"
              .= domainText host
              ~~ msg (val "awsEnvForHost - Looking up multiIngress config.")
          case multiIngressConf ^. at host of
            Nothing -> do
              Log.debug $
                "host"
                  .= domainText host
                  ~~ msg (val "awsEnvForHost - multiIngress lookup failed, no config for provided Z-Host header.")
              throwE noMatchingAssetEndpoint
            Just hostAwsEnv -> do
              Log.debug $
                "host"
                  .= domainText host
                  ~~ "s3DownloadEndpoint"
                    .= show hostAwsEnv.amazonkaDownloadEndpoint
                  ~~ msg (val "awsEnvForHost - multiIngress lookup succeed, using specific AWS env.")
              pure hostAwsEnv

-- | 'False' if S3 answers 404, throws on other errors.
headObjectExists :: S3AssetKey -> Handler Bool
headObjectExists (S3AssetKey key) =
  execEither (\b -> newHeadObject (BucketName b) (ObjectKey key)) >>= \case
    Right _ -> pure True
    Left (ServiceError se) | se ^. serviceError_status == status404 -> pure False
    Left e -> throwM (AWS.GeneralError e)

-- | Common prefixes (delimiter @/@) directly below @prefix@, all pages.
listCommonPrefixes :: Text -> Handler [Text]
listCommonPrefixes prefix =
  concatMap (mapMaybe (view commonPrefix_prefix) . fold . view listObjectsV2Response_commonPrefixes)
    <$> listObjectsV2Pages prefix (Just '/')

-- | Keys and sizes of all objects below @prefix@, all pages.
listObjects :: Text -> Handler [(Text, Word64)]
listObjects prefix =
  concatMap (map entry . fold . view listObjectsV2Response_contents)
    <$> listObjectsV2Pages prefix Nothing
  where
    entry o = (o ^. object_key . _ObjectKey, fromIntegral (o ^. object_size))

listObjectsV2Pages :: Text -> Maybe Delimiter -> Handler [ListObjectsV2Response]
listObjectsV2Pages prefix delimiter = go Nothing
  where
    go token = do
      r <-
        exec $ \b ->
          newListObjectsV2 (BucketName b)
            & listObjectsV2_prefix ?~ prefix
            & listObjectsV2_delimiter .~ delimiter
            & listObjectsV2_continuationToken .~ token
      case (r ^. listObjectsV2Response_isTruncated, r ^. listObjectsV2Response_nextContinuationToken) of
        (Just True, Just next) -> (r :) <$> go (Just next)
        _ -> pure [r]

-- | Deletes /all versions/ (and delete markers) of the objects below @prefix@ whose key passes
-- @keep@. On a versioned bucket a plain delete only adds a delete marker, the encrypted data
-- stays and is restorable. The RFC requires backups of a removed device to be deleted
-- immediately.
--
-- Stores without @ListObjectVersions@ (S3 error @NotImplemented@) are unversioned: falls back
-- to plain deletes of the listed objects.
deleteAllVersions :: Text -> (Text -> Bool) -> Handler ()
deleteAllVersions prefix keep = do
  versions <-
    listVersions prefix >>= \case
      Right vs -> pure vs
      Left (ServiceError se) | se ^. serviceError_code == ErrorCode "NotImplemented" -> do
        Log.warn $
          "remote" .= val "S3"
            ~~ msg (val "ListObjectVersions not implemented, deleting without version ids")
        map ((,Nothing) . fst) <$> listObjects prefix
      Left e -> throwM (AWS.GeneralError e)
  for_ (chunksOf 1000 (filter (keep . fst) versions)) deleteObjects
  where
    chunksOf n xs = case splitAt n xs of
      ([], _) -> []
      (c, rest) -> c : chunksOf n rest

-- | All (key, version id) pairs below @prefix@, versions and delete markers, all pages.
listVersions :: Text -> Handler (Either Amazonka.Error [(Text, Maybe ObjectVersionId)])
listVersions prefix = go Nothing Nothing
  where
    go keyMarker versionMarker = do
      res <-
        execEither $ \b ->
          newListObjectVersions (BucketName b)
            & listObjectVersions_prefix ?~ prefix
            & listObjectVersions_keyMarker .~ keyMarker
            & listObjectVersions_versionIdMarker .~ versionMarker
      case res of
        Left e -> pure (Left e)
        Right r -> do
          let entries =
                [ (k ^. _ObjectKey, v ^. objectVersion_versionId)
                | v <- fold (r ^. listObjectVersionsResponse_versions),
                  k <- toList (v ^. objectVersion_key)
                ]
                  <> [ (k ^. _ObjectKey, m ^. deleteMarkerEntry_versionId)
                     | m <- fold (r ^. listObjectVersionsResponse_deleteMarkers),
                       k <- toList (m ^. deleteMarkerEntry_key)
                     ]
              markers =
                (,)
                  <$> r ^. listObjectVersionsResponse_nextKeyMarker
                  <*> r ^. listObjectVersionsResponse_nextVersionIdMarker
          case (r ^. listObjectVersionsResponse_isTruncated, markers) of
            (Just True, Just (k, v)) -> fmap (entries <>) <$> go (Just k) (Just v)
            (Just True, Nothing) -> do
              -- Stopping here would leave versions behind.
              Log.err $
                "remote" .= val "S3"
                  ~~ "prefix" .= prefix
                  ~~ msg (val "ListObjectVersions truncated without next markers")
              throwE serverError
            _ -> pure (Right entries)

-- | One @DeleteObjects@ request (at most 1000 objects), fails on any per-object error.
deleteObjects :: [(Text, Maybe ObjectVersionId)] -> Handler ()
deleteObjects objs = do
  let ids = [newObjectIdentifier (ObjectKey k) & objectIdentifier_versionId .~ v | (k, v) <- objs]
      del = newDelete & delete_objects .~ ids & delete_quiet ?~ True
  r <- exec $ \b -> newDeleteObjects (BucketName b) del
  let errs = fold (r ^. deleteObjectsResponse_errors)
  unless (null errs) $ do
    Log.err $
      "remote" .= val "S3"
        ~~ "errors" .= show errs
        ~~ msg (val "Failed to delete backup objects")
    throwE serverError

-- | Signed backup download URL from the default AWS environment, also with @aws.multiIngress@:
-- backup routes have no @Z-Host@ to pick an ingress.
signedBackupURL :: S3AssetKey -> Handler URI
signedBackupURL key = asks (.aws) >>= \e -> presignedURL e key

presignedURL :: (ToByteString p) => AWS.Env -> p -> Handler URI
presignedURL e path = do
  now <- liftIO getCurrentTime
  ttl <- asks (.options.settings.downloadLinkTTL)
  let key = ObjectKey . Text.decodeLatin1 $ toByteString' path
      req = newGetObject (BucketName e.s3Bucket) key
  signed <-
    presignURL (amazonkaEnvWithDownloadEndpoint e) now (Seconds (fromIntegral ttl)) req
  case parseURI strictURIParserOptions signed of
    Left err -> do
      Log.err $
        "remote" .= val "S3"
          ~~ "error" .= show err
          ~~ msg (val "Failed to generate a signed URI")
      throwE serverError
    Right u -> pure u

mkKey :: V3.AssetKey -> S3AssetKey
mkKey (V3.AssetKeyV3 i r) = S3AssetKey $ "v3/" <> retention <> "/" <> key
  where
    key = UUID.toText (toUUID i)
    retention = V3.retentionToTextRep r

metaHeaders :: Maybe V3.AssetToken -> V3.Principal -> Maybe AssetAuditLogMetadata -> HML.HashMap Text Text
metaHeaders tok prc wireMeta =
  HML.fromList $
    catMaybes
      [ setAmzMetaToken <$> tok,
        Just (setAmzMetaPrincipal prc),
        setAmzAuditLogMetadata <$> wireMeta
      ]

-------------------------------------------------------------------------------
-- S3 Metadata Headers

hAmzMetaUser :: Text
hAmzMetaUser = "user"

hAmzMetaBot :: Text
hAmzMetaBot = "bot"

hAmzMetaProvider :: Text
hAmzMetaProvider = "provider"

hAmzMetaToken :: Text
hAmzMetaToken = "token"

hAmzWireMetadata :: Text
hAmzWireMetadata = "wire-metadata"

-------------------------------------------------------------------------------
-- S3 Metadata Setters

setAmzMetaUser :: UserId -> (Text, Text)
setAmzMetaUser u = (hAmzMetaUser, UUID.toText (toUUID u))

setAmzMetaBot :: BotId -> (Text, Text)
setAmzMetaBot b = (hAmzMetaBot, UUID.toText (toUUID (botUserId b)))

setAmzMetaProvider :: ProviderId -> (Text, Text)
setAmzMetaProvider p = (hAmzMetaProvider, UUID.toText (toUUID p))

setAmzMetaToken :: V3.AssetToken -> (Text, Text)
setAmzMetaToken t = (hAmzMetaToken, Ascii.toText (V3.assetTokenAscii t))

setAmzMetaPrincipal :: V3.Principal -> (Text, Text)
setAmzMetaPrincipal (V3.UserPrincipal u) = setAmzMetaUser u
setAmzMetaPrincipal (V3.BotPrincipal b) = setAmzMetaBot b
setAmzMetaPrincipal (V3.ProviderPrincipal p) = setAmzMetaProvider p

setAmzAuditLogMetadata :: AssetAuditLogMetadata -> (Text, Text)
setAmzAuditLogMetadata t = (hAmzWireMetadata, encodeAuditLogMetadata t)
  where
    encodeAuditLogMetadata :: AssetAuditLogMetadata -> Text
    encodeAuditLogMetadata meta =
      decodeLatin1 . HTTPURI.urlEncode False . LBS.toStrict $ A.encode meta

-------------------------------------------------------------------------------
-- S3 Metadata Getters

lookupCI :: (CI.FoldCase a, Eq a) => a -> [(a, b)] -> Maybe b
lookupCI k = lookup (CI.mk k) . fmap (first CI.mk)

getAmzMetaPrincipal :: [(Text, Text)] -> Maybe V3.Principal
getAmzMetaPrincipal h =
  (V3.UserPrincipal <$> getAmzMetaUser h)
    <|> (V3.BotPrincipal <$> getAmzMetaBot h)
    <|> (V3.ProviderPrincipal <$> getAmzMetaProvider h)

getAmzMetaUser :: [(Text, Text)] -> Maybe UserId
getAmzMetaUser = parseAmzMeta hAmzMetaUser

getAmzMetaBot :: [(Text, Text)] -> Maybe BotId
getAmzMetaBot = parseAmzMeta hAmzMetaBot

getAmzMetaProvider :: [(Text, Text)] -> Maybe ProviderId
getAmzMetaProvider = parseAmzMeta hAmzMetaProvider

getAmzMetaToken :: [(Text, Text)] -> Maybe V3.AssetToken
getAmzMetaToken h =
  V3.AssetToken . Ascii.unsafeFromText
    <$> lookupCI hAmzMetaToken h

parseAmzMeta :: (FromByteString a) => Text -> [(Text, Text)] -> Maybe a
parseAmzMeta k h = lookupCI k h >>= fromByteString . encodeUtf8

getAmzAuditLogMetadata :: [(Text, Text)] -> Maybe AssetAuditLogMetadata
getAmzAuditLogMetadata = lookupCI hAmzWireMetadata >=> parseAuditLogMetadata
  where
    parseAuditLogMetadata :: Text -> Maybe AssetAuditLogMetadata
    parseAuditLogMetadata t = parseJSON t <|> parseJSONBytes (HTTPURI.urlDecode False (encodeUtf8 t))

    parseJSON :: Text -> Maybe AssetAuditLogMetadata
    parseJSON = parseJSONBytes . encodeUtf8

    parseJSONBytes :: ByteString -> Maybe AssetAuditLogMetadata
    parseJSONBytes = A.decode . fromStrict

-------------------------------------------------------------------------------
-- Utilities

octets :: MIME.Type
octets = MIME.Type (MIME.Application "octet-stream") []

exec ::
  ( AWSRequest r,
    Show r
  ) =>
  (Text -> r) ->
  Handler (AWSResponse r)
exec req = do
  env <- asks (.aws)
  AWS.exec env req

-- | Like 'exec', but returns errors instead of throwing them.
execEither ::
  (AWSRequest r) =>
  (Text -> r) ->
  Handler (Either Amazonka.Error (AWSResponse r))
execEither req = do
  env <- asks (.aws)
  AWS.execute env (AWS.sendCatch env.amazonkaEnv (req env.s3Bucket))

execCatch ::
  ( AWSRequest r,
    Show r
  ) =>
  (Text -> r) ->
  Handler (Maybe (AWSResponse r))
execCatch req = do
  env <- asks (.aws)
  AWS.execCatch env req

--------------------------------------------------------------------------------
-- Legacy

plainKey :: AssetId -> S3AssetKey
plainKey a = S3AssetKey $ Text.pack (show a)

otrKey :: ConvId -> AssetId -> S3AssetKey
otrKey c a = S3AssetKey $ "otr/" <> Text.pack (show c) <> "/" <> Text.pack (show a)

getMetadata :: AssetId -> Handler (Maybe Bool)
getMetadata ast = do
  r <- execCatch req
  pure $ (parse <$> HML.toList) . view headObjectResponse_metadata <$> r
  where
    req b = newHeadObject (BucketName b) (ObjectKey . Text.pack $ show ast)
    parse =
      maybe False (Text.isInfixOf "public=true" . Text.toLower)
        . lookupCI "zasset"

getOtrMetadata :: ConvId -> AssetId -> Handler (Maybe UserId)
getOtrMetadata cnv ast = do
  let S3AssetKey key = otrKey cnv ast
  r <- execCatch (req key)
  pure $ getAmzMetaUser . (HML.toList <$> view headObjectResponse_metadata) =<< r
  where
    req k b = newHeadObject (BucketName b) (ObjectKey k)

-- Arbitrary instance for property-based testing
instance Arbitrary AssetAuditLogMetadata where
  arbitrary =
    AssetAuditLogMetadata
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
