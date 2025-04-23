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
  ( S3AssetKey,
    S3AssetMeta (..),
    uploadV3,
    downloadV3,
    getMetadataV3,
    updateMetadataV3,
    deleteV3,
    mkKey,
    signedURL,
    -- Legacy
    plainKey,
    otrKey,
    getMetadata,
    getOtrMetadata,
  )
where

import Amazonka hiding (Error)
import Amazonka.S3
import Amazonka.S3.Lens
import qualified Amazonka.S3.StreamingUpload as SU
import CargoHold.API.Error
import CargoHold.AWS (amazonkaEnvWithDownloadEndpoint)
import qualified CargoHold.AWS as AWS
import CargoHold.App hiding (Env, Handler)
import CargoHold.Options
import qualified CargoHold.Types.V3 as V3
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Conduit
import Control.Error (ExceptT, throwE)
import Control.Lens hiding (parts, (.=), (:<), (:>))
import Control.Monad.Catch (try)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Conduit.Binary
import qualified Data.HashMap.Lazy as HML
import Data.Id
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import qualified Data.UUID as UUID
import Imports
import Network.Wai.Utilities.Error (Error (..))
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (.=), (~~))
import URI.ByteString

newtype S3AssetKey = S3AssetKey {s3Key :: Text}
  deriving (Eq, Show, ToByteString)

-- | Asset metadata tracked in S3.
data S3AssetMeta = S3AssetMeta
  { v3AssetOwner :: V3.Principal,
    v3AssetToken :: Maybe V3.AssetToken,
    v3AssetType :: MIME.Type -- should be ignored, see note on overrideMimeTypeAsOctetStream. FUTUREWORK: remove entirely.
  }
  deriving (Show)

-- [Note: overrideMimeTypeAsOctetStream]
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
  Maybe V3.AssetToken ->
  -- | streaming payload
  ConduitM () ByteString (ResourceT IO) () ->
  ExceptT Error App ()
uploadV3 prc (s3Key . mkKey -> key) (V3.AssetHeaders _ cl) tok src = do
  Log.info $
    "remote" .= val "S3"
      ~~ "asset.owner" .= toByteString prc
      ~~ "asset.key" .= key
      ~~ "asset.type" .= MIME.showType ct
      ~~ "asset.size" .= cl
      ~~ msg (val "Uploading asset")

  awsEnv <- asks (.aws)

  -- build the “initiate multipart” request
  let createReq =
        newCreateMultipartUpload (BucketName awsEnv.s3Bucket) (ObjectKey key)
          & createMultipartUpload_contentType ?~ MIME.showType ct
          & createMultipartUpload_metadata .~ metaHeaders tok prc

  cntRef <- liftIO $ newIORef (0 :: Int)

  -- byte counter for validating that an upload was completely uploaded from the client
  let countC = awaitForever $ \bs -> do
        liftIO $ modifyIORef' cntRef (+ BS.length bs)
        yield bs

  result <-
    liftIO . try . runResourceT . runConduit $
      src
        .| chunksOfCE (fromIntegral defaultChunkSize)
        .| isolate (fromIntegral cl)
        .| countC
        .| SU.streamUpload awsEnv.amazonkaEnv Nothing createReq

  case result of
    Left (e :: SomeException) -> do
      Log.err $
        "remote" .= val "S3"
          ~~ "asset.owner" .= toByteString prc
          ~~ "asset.key" .= key
          ~~ "asset.type" .= MIME.showType ct
          ~~ "asset.size" .= cl
          ~~ msg ("Unhandled error while uploading asset: " <> show e)
      throwE serverError
    Right uploadResult -> do

      bytesSeen <- liftIO $ readIORef cntRef
      when (bytesSeen /= (fromIntegral cl)) $ do
        throwE incompleteBody

      case uploadResult of
        Right (_ :: CompleteMultipartUploadResponse) -> pure ()
        Left ((abortResponse, exception) :: (AbortMultipartUploadResponse, SomeException)) -> do
          Log.err $
            "remote" .= val "S3"
              ~~ "asset.owner" .= toByteString prc
              ~~ "asset.key" .= key
              ~~ "asset.type" .= MIME.showType ct
              ~~ "asset.size" .= cl
              ~~ msg ("Multipart upload failed – aborted: " <> (show (abortResponse, exception)))
          throwE serverError
  where
    ct = octets -- see note “overrideMimeTypeAsOctetStream”

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
  ExceptT Error App (ConduitM () ByteString (ResourceT IO) ())
downloadV3 (s3Key . mkKey -> key) = do
  env <- asks (.aws)
  pure . flattenResourceT $ view (getObjectResponse_body . _ResponseBody) <$> AWS.execStream env req
  where
    req :: Text -> GetObject
    req b =
      newGetObject (BucketName b) (ObjectKey key)
        & getObject_responseContentType ?~ MIME.showType octets

getMetadataV3 :: V3.AssetKey -> ExceptT Error App (Maybe S3AssetMeta)
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

deleteV3 :: V3.AssetKey -> ExceptT Error App ()
deleteV3 (s3Key . mkKey -> key) = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.key" .= key
      ~~ msg (val "Deleting asset")
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.key" .= key
      ~~ msg (val "Deleting asset")
  void $ exec req
  where
    req b = newDeleteObject (BucketName b) (ObjectKey key)

updateMetadataV3 :: V3.AssetKey -> S3AssetMeta -> ExceptT Error App ()
updateMetadataV3 (s3Key . mkKey -> key) (S3AssetMeta prc tok _) = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.owner" .= show prc
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
        & copyObject_metadata .~ metaHeaders tok prc

-- | Generate an `URI` for asset download redirects
--
-- If @aws.multiIngress@ is configured, the endpoint is looked up from this
-- `Map` with the @Z-Host@ header's value as key. Otherwise (the default case
-- that applies to most deployments), use the default AWS environment; i.e. the
-- environment with @aws.s3DownloadEndpoint@.
signedURL :: (ToByteString p) => p -> Maybe Text -> ExceptT Error App URI
signedURL path mbHost = do
  e <- awsEnvForHost
  now <- liftIO getCurrentTime
  ttl <- asks (.options.settings.downloadLinkTTL)
  let req = newGetObject (BucketName e.s3Bucket) (ObjectKey . Text.decodeLatin1 $ toByteString' path)
  signed <-
    presignURL (amazonkaEnvWithDownloadEndpoint e) now (Seconds (fromIntegral ttl)) req
  toUri signed
  where
    toUri x = case parseURI strictURIParserOptions x of
      Left e -> do
        Log.err $
          "remote" .= val "S3"
            ~~ "error" .= show e
            ~~ msg (val "Failed to generate a signed URI")
        throwE serverError
      Right u -> pure u

    awsEnvForHost :: ExceptT Error App AWS.Env
    awsEnvForHost = do
      multiIngressConf <- asks (.multiIngress)
      if null multiIngressConf
        then asks (.aws)
        else awsEnvForHost' mbHost multiIngressConf
      where
        awsEnvForHost' :: Maybe Text -> Map String AWS.Env -> ExceptT Error App AWS.Env
        awsEnvForHost' Nothing _ = do
          Log.debug $
            msg (val "awsEnvForHost - multiIngress configured, but no Z-Host header provided.")
          throwE noMatchingAssetEndpoint
        awsEnvForHost' (Just host) multiIngressConf = do
          Log.debug $
            "host"
              .= host
              ~~ msg (val "awsEnvForHost - Looking up multiIngress config.")
          case multiIngressConf ^. at (Text.unpack host) of
            Nothing -> do
              Log.debug $
                "host"
                  .= host
                  ~~ msg (val "awsEnvForHost - multiIngress lookup failed, no config for provided Z-Host header.")
              throwE noMatchingAssetEndpoint
            Just hostAwsEnv -> do
              Log.debug $
                "host"
                  .= host
                  ~~ "s3DownloadEndpoint"
                    .= show hostAwsEnv.amazonkaDownloadEndpoint
                  ~~ msg (val "awsEnvForHost - multiIngress lookup succeed, using specific AWS env.")
              pure hostAwsEnv

mkKey :: V3.AssetKey -> S3AssetKey
mkKey (V3.AssetKeyV3 i r) = S3AssetKey $ "v3/" <> retention <> "/" <> key
  where
    key = UUID.toText (toUUID i)
    retention = V3.retentionToTextRep r

metaHeaders :: Maybe V3.AssetToken -> V3.Principal -> HML.HashMap Text Text
metaHeaders tok prc =
  HML.fromList $
    catMaybes
      [ setAmzMetaToken <$> tok,
        Just (setAmzMetaPrincipal prc)
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

-------------------------------------------------------------------------------
-- Utilities

octets :: MIME.Type
octets = MIME.Type (MIME.Application "octet-stream") []

exec ::
  ( AWSRequest r,
    Typeable r,
    Typeable (AWSResponse r),
    Show r
  ) =>
  (Text -> r) ->
  ExceptT Error App (AWSResponse r)
exec req = do
  env <- asks (.aws)
  AWS.exec env req

execCatch ::
  ( AWSRequest r,
    Typeable r,
    Typeable (AWSResponse r),
    Show r
  ) =>
  (Text -> r) ->
  ExceptT Error App (Maybe (AWSResponse r))
execCatch req = do
  env <- asks (.aws)
  AWS.execCatch env req

--------------------------------------------------------------------------------
-- Legacy

plainKey :: AssetId -> S3AssetKey
plainKey a = S3AssetKey $ Text.pack (show a)

otrKey :: ConvId -> AssetId -> S3AssetKey
otrKey c a = S3AssetKey $ "otr/" <> Text.pack (show c) <> "/" <> Text.pack (show a)

getMetadata :: AssetId -> ExceptT Error App (Maybe Bool)
getMetadata ast = do
  r <- execCatch req
  pure $ (parse <$> HML.toList) . view headObjectResponse_metadata <$> r
  where
    req b = newHeadObject (BucketName b) (ObjectKey . Text.pack $ show ast)
    parse =
      maybe False (Text.isInfixOf "public=true" . Text.toLower)
        . lookupCI "zasset"

getOtrMetadata :: ConvId -> AssetId -> ExceptT Error App (Maybe UserId)
getOtrMetadata cnv ast = do
  let S3AssetKey key = otrKey cnv ast
  r <- execCatch (req key)
  pure $ getAmzMetaUser . (HML.toList <$> view headObjectResponse_metadata) =<< r
  where
    req k b = newHeadObject (BucketName b) (ObjectKey k)
