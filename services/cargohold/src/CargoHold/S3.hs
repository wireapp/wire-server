{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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

module CargoHold.S3
  ( S3AssetKey,
    S3AssetMeta (..),
    uploadV3,
    getMetadataV3,
    updateMetadataV3,
    deleteV3,
    mkKey,
    signedURL,

    -- * Resumable Uploads
    S3Resumable,
    resumableOwner,
    resumableTotalSize,
    resumableExpires,
    resumableChunkSize,
    resumableOffset,
    createResumable,
    getResumable,
    completeResumable,
    S3Chunk,
    uploadChunk,
    -- Legacy
    plainKey,
    otrKey,
    getMetadata,
    getOtrMetadata,
  )
where

import CargoHold.API.Error
import qualified CargoHold.AWS as AWS
import CargoHold.App hiding (Env, Handler)
import CargoHold.Options
import qualified CargoHold.Types.V3 as V3
import qualified CargoHold.Types.V3.Resumable as V3
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Conduit
import Control.Error (ExceptT, throwE)
import Control.Lens hiding (parts, (.=), (:<), (:>))
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Binary as Conduit
import qualified Data.HashMap.Lazy as HML
import Data.Id
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import qualified Data.UUID as UUID
import Imports
import Network.AWS hiding (Error)
import Network.AWS.Data.Body
import Network.AWS.Data.Crypto
import Network.AWS.S3
import Network.Wai.Utilities.Error (Error (..))
import Safe (readMay)
import qualified System.Logger.Class as Log
import System.Logger.Message (msg, val, (.=), (~~))
import URI.ByteString

newtype S3AssetKey = S3AssetKey {s3Key :: Text}
  deriving (Eq, Show, ToByteString)

-- | Asset metadata tracked in S3.
data S3AssetMeta = S3AssetMeta
  { v3AssetOwner :: V3.Principal,
    v3AssetToken :: Maybe V3.AssetToken,
    v3AssetType :: MIME.Type
  }
  deriving (Show)

uploadV3 ::
  V3.Principal ->
  V3.AssetKey ->
  V3.AssetHeaders ->
  Maybe V3.AssetToken ->
  Conduit.ConduitM () ByteString (ResourceT IO) () ->
  ExceptT Error App ()
uploadV3 prc (s3Key . mkKey -> key) (V3.AssetHeaders ct cl md5) tok src = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.owner" .= toByteString prc
      ~~ "asset.key" .= key
      ~~ "asset.type" .= MIME.showType ct
      ~~ "asset.size" .= cl
      ~~ msg (val "Uploading asset")
  void $ exec req
  where
    stream =
      src
        -- Rechunk bytestream to ensure we satisfy AWS's minimum chunk size
        -- on uploads
        .| Conduit.chunksOfCE (fromIntegral defaultChunkSize)
        -- Ignore any 'junk' after the content; take only 'cl' bytes.
        .| Conduit.isolate (fromIntegral cl)
    reqBdy = ChunkedBody defaultChunkSize (fromIntegral cl) stream
    md5Res = Text.decodeLatin1 $ digestToBase Base64 md5
    req b =
      putObject (BucketName b) (ObjectKey key) (toBody reqBdy)
        & poContentType ?~ MIME.showType ct
        & poContentMD5 ?~ md5Res
        & poMetadata .~ metaHeaders tok prc

getMetadataV3 :: V3.AssetKey -> ExceptT Error App (Maybe S3AssetMeta)
getMetadataV3 (s3Key . mkKey -> key) = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.key" .= key
      ~~ msg
        (val "Getting asset metadata")
  maybe (return Nothing) handle =<< execCatch req
  where
    req b = headObject (BucketName b) (ObjectKey key)
    handle r = do
      let ct = fromMaybe octets (MIME.parseMIMEType =<< r ^. horsContentType)
      let meta = HML.toList $ r ^. horsMetadata
      return $ parse ct meta
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
    req b = deleteObject (BucketName b) (ObjectKey key)

updateMetadataV3 :: V3.AssetKey -> S3AssetMeta -> ExceptT Error App ()
updateMetadataV3 (s3Key . mkKey -> key) (S3AssetMeta prc tok ct) = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset.owner" .= show prc
      ~~ "asset.key" .= key
      ~~ msg (val "Updating asset metadata")
  void $ exec req
  where
    copySrc b =
      decodeLatin1 . LBS.toStrict . toLazyByteString $
        urlEncode [] $
          Text.encodeUtf8 (b <> "/" <> key)
    req b =
      copyObject (BucketName b) (copySrc b) (ObjectKey key)
        & coContentType ?~ MIME.showType ct
        & coMetadataDirective ?~ MDReplace
        & coMetadata .~ metaHeaders tok prc

signedURL :: (ToByteString p) => p -> ExceptT Error App URI
signedURL path = do
  e <- view aws
  let b = view AWS.s3Bucket e
  now <- liftIO getCurrentTime
  ttl <- view (settings . setDownloadLinkTTL)
  let req = getObject (BucketName b) (ObjectKey . Text.decodeLatin1 $ toByteString' path)
  signed <-
    AWS.execute (AWS.useDownloadEndpoint e) $
      presignURL now (Seconds (fromIntegral ttl)) req
  toUri signed
  where
    toUri x = case parseURI strictURIParserOptions x of
      Left e -> do
        Log.err $
          "remote" .= val "S3"
            ~~ msg (val "Failed to generate a signed URI")
            ~~ msg (show e)
        throwE serverError
      Right u -> return u

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
-- Resumable Uploads

newtype S3ResumableKey = S3ResumableKey {s3ResumableKey :: Text}
  deriving (Eq, Show, ToByteString)

newtype S3ChunkKey = S3ChunkKey {s3ChunkKey :: Text}
  deriving (Eq, Show, ToByteString)

newtype S3ChunkNr = S3ChunkNr Word
  deriving (Eq, Ord, Show, ToByteString, FromByteString, Num, Integral, Enum, Real)

newtype S3ETag = S3ETag {s3ETag :: Text}
  deriving (Eq, Show, ToByteString, FromByteString)

data S3Resumable = S3Resumable
  { -- | The resumable asset key.
    resumableKey :: S3ResumableKey,
    -- | The final asset key.
    resumableAsset :: V3.AssetKey,
    -- | The creator (i.e. owner).
    resumableOwner :: V3.Principal,
    -- | Size of each chunk.
    resumableChunkSize :: V3.ChunkSize,
    -- | Size of the final asset.
    resumableTotalSize :: V3.TotalSize,
    -- | MIME type of the final asset.
    resumableType :: MIME.Type,
    -- | Token of the final asset.
    resumableToken :: Maybe V3.AssetToken,
    -- | Expiry of the resumable upload.
    resumableExpires :: UTCTime,
    -- | S3 multipart upload ID, if any.
    resumableUploadId :: Maybe Text,
    resumableChunks :: Seq S3Chunk
  }
  deriving (Show)

data S3Chunk = S3Chunk
  { -- | Sequence nr.
    chunkNr :: S3ChunkNr,
    -- | Offset of the first byte.
    chunkOffset :: V3.Offset,
    -- | (Actual) Size of the chunk.
    chunkSize :: Word,
    -- | S3 ETag.
    chunkETag :: S3ETag
  }
  deriving (Show)

mkChunkNr :: S3Resumable -> V3.Offset -> S3ChunkNr
mkChunkNr r o = S3ChunkNr ((offBytes `quot` chunkBytes) + 1)
  where
    offBytes = V3.offsetBytes o
    chunkBytes = V3.chunkSizeBytes (resumableChunkSize r)

mkOffset :: S3Resumable -> S3ChunkNr -> V3.Offset
mkOffset r n = V3.Offset ((fromIntegral n - 1) * chunkBytes)
  where
    chunkBytes = V3.chunkSizeBytes (resumableChunkSize r)

resumableOffset :: S3Resumable -> V3.Offset
resumableOffset r = case Seq.viewr (resumableChunks r) of
  Seq.EmptyR -> V3.Offset 0
  _ :> c -> chunkOffset c + V3.Offset (chunkSize c)

-- | Given a total size for an upload, calculates the desired
-- size of individual chunks. Semantically, the calculation grows
-- the number of chunks and the chunk size in an alternating fashion
-- until the number of chunks multiplied by the chunk size is equal
-- or greater than the given total size:
--
-- [0. If the total size is less than 'minSmallSize', then 'minSmallSize'
--    is the chunk size and we are done.]
-- 1. Starting with a chunk size of 'minSmallSize', the number
--    of chunks is increased up to 'maxSmallChunks'.
-- 2. Staying at 'maxSmallChunks', the chunk size is increased
--    up to 'maxSmallSize'.
-- 3. Starting with a chunk size of 'minBigSize' and 1 chunk, the number
--    of chunks is increased up to 'maxTotalChunks'.
-- 4. Staying at 'maxTotalChunks', the chunk size is increased
--    until the total size is accommodated.
calculateChunkSize :: V3.TotalSize -> V3.ChunkSize
calculateChunkSize (fromIntegral -> total) =
  let smallChunks = max 1 (min maxSmallChunks (total `quot` minSmallSize))
      bigChunks = max 1 (min maxTotalChunks (total `quot` minBigSize))
      smallSize = total `quot` smallChunks
      bigSize = total `quot` bigChunks
   in V3.ChunkSize $
        if
            | smallChunks < maxSmallChunks -> minSmallSize
            | smallSize <= maxSmallSize -> smallSize
            | bigChunks < maxTotalChunks -> minBigSize
            | otherwise -> bigSize

-- | The maximum number of small chunks, sized ['minSmallChunk', 'maxSmallChunk']
-- that we are willing to assemble on our side, to compensate for the 5MiB lower
-- bound on S3 multipart uploads.
maxSmallChunks :: Word
maxSmallChunks = 25

-- | The maximum number of chunks we are willing to process in total for a
-- single upload, regardless of where the final assembly is performed.
maxTotalChunks :: Word
maxTotalChunks = 1000

-- | Lower bound (inclusive) for small chunks.
minSmallSize :: Word
minSmallSize = 100 * 1024 -- 100 KiB

-- | Upper bound (inclusive) for small chunks.
maxSmallSize :: Word
maxSmallSize = 1 * 1024 * 1024 -- 1 MiB

-- | Lower bound (inclusive) for large chunks, i.e. the lower bound for S3
-- multipart upload uploads.
minBigSize :: Word
minBigSize = 5 * 1024 * 1024 -- 5 MiB

getResumable :: V3.AssetKey -> ExceptT Error App (Maybe S3Resumable)
getResumable k = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset" .= toByteString k
      ~~ "asset.key" .= toByteString rk
      ~~ "asset.key.meta" .= toByteString mk
      ~~ msg (val "Getting resumable asset metadata")
  maybe (return Nothing) handle =<< execCatch req
  where
    rk = mkResumableKey k
    mk = mkResumableKeyMeta k
    req b = headObject (BucketName b) (ObjectKey $ s3ResumableKey mk)
    handle r = do
      let ct = fromMaybe octets (MIME.parseMIMEType =<< view horsContentType r)
      let meta = HML.toList $ view horsMetadata r
      case parse ct meta of
        Nothing -> return Nothing
        Just r' -> fmap (\cs -> r' {resumableChunks = cs}) <$> listChunks r'
    parse ct h =
      S3Resumable rk k
        <$> getAmzMetaPrincipal h
        <*> getAmzMetaChunkSize h
        <*> getAmzMetaTotalSize h
        <*> pure ct
        <*> Just (getAmzMetaToken h)
        <*> getAmzMetaUploadExpires h
        <*> Just (getAmzMetaUploadId h)
        <*> pure Seq.empty

createResumable ::
  V3.AssetKey ->
  V3.Principal ->
  MIME.Type ->
  V3.TotalSize ->
  Maybe V3.AssetToken ->
  ExceptT Error App S3Resumable
createResumable k p typ size tok = do
  let csize = calculateChunkSize size
  ex <- addUTCTime V3.assetVolatileSeconds <$> liftIO getCurrentTime
  let key = mkResumableKey k
      mk = mkResumableKeyMeta k
  let res = S3Resumable key k p csize size typ tok ex Nothing Seq.empty
  up <- initMultipart res
  let ct = resumableType res
  void . exec $ first (s3ResumableKey mk) ct (resumableMeta csize ex up)
  return res {resumableUploadId = up}
  where
    initMultipart r
      | canUseMultipart r = do
        let cmu b =
              createMultipartUpload (BucketName b) (ObjectKey $ s3Key (mkKey k))
                & cmuContentType ?~ MIME.showType (resumableType r)
                & cmuMetadata .~ metaHeaders (resumableToken r) p
        imur <- exec cmu
        return $! view cmursUploadId imur
      | otherwise = return Nothing
    first key ct meta b =
      putObject (BucketName b) (ObjectKey key) (toBody (mempty :: ByteString))
        & poContentType ?~ MIME.showType ct
        & poMetadata .~ HML.fromList meta
    -- Determine whether a given 'S3Resumable' is eligible for the
    -- S3 multipart upload API. That is the case if the chunk size
    -- is >= 5 MiB or if there is only 1 chunk (<= 'minSmallSize').
    canUseMultipart r = chunkBytes >= minBigSize || totalBytes <= minSmallSize
      where
        chunkBytes = V3.chunkSizeBytes (resumableChunkSize r)
        totalBytes = V3.totalSizeBytes (resumableTotalSize r)
    resumableMeta csize expires upl =
      setAmzMetaPrincipal p :
      setAmzMetaTotalSize size :
      setAmzMetaChunkSize csize :
      setAmzMetaUploadExpires expires :
      catMaybes
        [ setAmzMetaToken <$> tok,
          setAmzMetaUploadId <$> upl
        ]

uploadChunk ::
  S3Resumable ->
  V3.Offset ->
  Conduit.SealedConduitT () ByteString IO () ->
  ExceptT Error App (S3Resumable, Conduit.SealedConduitT () ByteString IO ())
uploadChunk r offset rsrc = do
  let chunkSize = fromIntegral (resumableChunkSize r)
  (rest, chunk) <- liftIO $ rsrc $$++ Conduit.take chunkSize
  let size = fromIntegral (LBS.length chunk)
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset" .= toByteString (resumableAsset r)
      ~~ "asset.owner" .= toByteString (resumableOwner r)
      ~~ "asset.key" .= toByteString (resumableKey r)
      ~~ "asset.chunk" .= toByteString nr
      ~~ "asset.offset" .= toByteString offset
      ~~ "asset.size" .= toByteString size
      ~~ msg (val "Uploading chunk")
  c <- case resumableUploadId r of
    Nothing -> putChunk chunk size
    Just up -> putPart up chunk size
  let r' = r {resumableChunks = resumableChunks r Seq.|> c}
  return (r', rest)
  where
    nr = mkChunkNr r offset
    ct = MIME.showType (resumableType r)
    putChunk chunk size = do
      let S3ChunkKey k = mkChunkKey (resumableKey r) nr
      let req b =
            putObject (BucketName b) (ObjectKey k) (toBody chunk)
              & poContentType ?~ ct
      void $ exec req
      return $! S3Chunk nr offset size (S3ETag "")
    putPart up chunk size = do
      let S3AssetKey k = mkKey (resumableAsset r)
      let req b =
            uploadPart (BucketName b) (ObjectKey k) (fromIntegral nr) up (toBody chunk)
      tg <- view uprsETag <$> exec req
      etag <- case tg of
        Just (ETag t) -> return $ S3ETag (Text.decodeLatin1 t)
        Nothing -> throwE serverError
      return $! S3Chunk nr offset size etag

-- | Complete a resumable upload, assembling all chunks into a final asset.
completeResumable :: S3Resumable -> ExceptT Error App ()
completeResumable r = do
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset" .= toByteString ast
      ~~ "asset.owner" .= toByteString own
      ~~ "asset.key" .= toByteString (resumableKey r)
      ~~ msg (val "Completing resumable upload")
  let chunks = resumableChunks r
  verifyChunks chunks
  case resumableUploadId r of
    Nothing -> assembleLocal chunks
    Just up -> assembleRemote up (NE.nonEmpty $ toList chunks)
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset" .= toByteString ast
      ~~ "asset.owner" .= toByteString own
      ~~ "asset.key" .= toByteString (resumableKey r)
      ~~ msg (val "Resumable upload completed")
  where
    (own, ast) = (resumableOwner r, resumableAsset r)
    -- Local assembly for small chunk sizes (< 5 MiB): Download and re-upload
    -- the chunks in a streaming fashion one-by-one to create the final object.
    assembleLocal :: Seq S3Chunk -> ExceptT Error App ()
    assembleLocal chunks = do
      e <- view aws
      let totalSize = fromIntegral (resumableTotalSize r)
      let chunkSize = calcChunkSize chunks
      let reqBdy = Chunked $ ChunkedBody chunkSize totalSize (chunkSource e chunks)
      let putRq b =
            putObject (BucketName b) (ObjectKey (s3Key (mkKey ast))) reqBdy
              & poContentType ?~ MIME.showType (resumableType r)
              & poMetadata .~ metaHeaders (resumableToken r) own
      void $ exec putRq

      -- For symmetry with the behavior of the S3 multipart API, where the
      -- resumable upload and all parts are removed upon completion, we do
      -- the same here.
      let rk = resumableKey r
      let keys =
            s3ResumableKey rk :
            map (s3ChunkKey . mkChunkKey rk . chunkNr) (toList chunks)
      let del =
            delete' & dObjects .~ map (objectIdentifier . ObjectKey) keys
              & dQuiet ?~ True
      let delRq b = deleteObjects (BucketName b) del
      void $ exec delRq

    -- All chunks except for the last should be of the same size so it makes
    -- sense to use that as our default
    calcChunkSize cs = case Seq.viewl cs of
      EmptyL -> defaultChunkSize
      c :< _ -> ChunkSize $ fromIntegral (chunkSize c)
    -- Remote assembly for large(r) chunk sizes (>= 5 MiB) via the
    -- S3 multipart upload API.
    assembleRemote _ Nothing = throwE serverError
    assembleRemote up (Just chunks) = do
      let key = s3Key (mkKey ast)
      let parts = fmap mkPart chunks
      let completeRq b =
            completeMultipartUpload (BucketName b) (ObjectKey key) up
              & cMultipartUpload ?~ (completedMultipartUpload & cmuParts ?~ parts)
      void $ exec completeRq
      let S3ResumableKey rkey = resumableKey r
      let delRq b = deleteObject (BucketName b) (ObjectKey rkey)
      void $ exec delRq
    mkPart c = completedPart (fromIntegral (chunkNr c)) (ETag . Text.encodeUtf8 $ s3ETag (chunkETag c))
    -- Verify that the chunks constitute the full asset, i.e. that the
    -- upload is complete.
    verifyChunks cs = do
      let !total = V3.TotalSize $ foldl' (\t v -> t + chunkSize v) 0 cs
      unless (total == resumableTotalSize r) $
        throwE $
          uploadIncomplete (resumableTotalSize r) total
    -- Construct a 'Source' by downloading the chunks.
    -- chunkSource :: AWS.Env
    --             -> Seq S3Chunk
    --             -> Source (ResourceT IO) ByteString
    chunkSource env cs = case Seq.viewl cs of
      EmptyL -> mempty
      c :< cc -> do
        let S3ChunkKey ck = mkChunkKey (resumableKey r) (chunkNr c)
        let b = view AWS.s3Bucket env
        let req = getObject (BucketName b) (ObjectKey ck)
        v <-
          lift $
            AWS.execute env $
              AWS.send req
                >>= flip sinkBody Conduit.sinkLbs . view gorsBody
        Conduit.yield (LBS.toStrict v) >> chunkSource env cc

listChunks :: S3Resumable -> ExceptT Error App (Maybe (Seq S3Chunk))
listChunks r = do
  let ast = resumableAsset r
  let S3ResumableKey key = resumableKey r
  Log.debug $
    "remote" .= val "S3"
      ~~ "asset" .= toByteString ast
      ~~ "asset.resumable" .= key
      ~~ msg (val "Listing chunks")
  fmap Seq.fromList <$> case resumableUploadId r of
    Nothing -> listBucket key
    Just up -> listMultiParts up
  where
    listBucket k = do
      let req b =
            listObjects (BucketName b)
              & loPrefix ?~ (k <> "/")
              & loMaxKeys ?~ fromIntegral maxTotalChunks
      maybe (return Nothing) parseObjects =<< execCatch req
    parseObjects =
      return . Just . mapMaybe chunkFromObject
        . view lorsContents
    listMultiParts up = do
      let req b =
            listParts
              (BucketName b)
              (ObjectKey $ s3Key (mkKey (resumableAsset r)))
              up
      maybe (return Nothing) parseParts =<< execCatch req
    parseParts =
      return . Just . mapMaybe chunkFromPart
        . view lprsParts
    chunkFromObject :: Object -> Maybe S3Chunk
    chunkFromObject o = do
      let (ObjectKey okey) = view oKey o
      nr <- parseNr okey
      let etag =
            let (ETag t) = (view oETag o)
             in S3ETag (Text.decodeLatin1 t)
      let size = fromIntegral (view oSize o)
      let off = mkOffset r nr
      Just $! S3Chunk nr off size etag
    chunkFromPart :: Part -> Maybe S3Chunk
    chunkFromPart p = case (view pPartNumber p, view pETag p, view pSize p) of
      (Just x, Just (ETag y), Just z) ->
        let nr = S3ChunkNr (fromIntegral x)
            off = mkOffset r nr
            size = (fromIntegral z)
            etag = S3ETag (Text.decodeLatin1 y)
         in Just $! S3Chunk nr off size etag
      _ -> Nothing
    parseNr = fmap S3ChunkNr . readMay . Text.unpack . snd . Text.breakOnEnd "/"

mkResumableKey :: V3.AssetKey -> S3ResumableKey
mkResumableKey (V3.AssetKeyV3 aid _) =
  S3ResumableKey $ "v3/resumable/" <> UUID.toText (toUUID aid)

mkResumableKeyMeta :: V3.AssetKey -> S3ResumableKey
mkResumableKeyMeta (V3.AssetKeyV3 aid _) =
  S3ResumableKey $ "v3/resumable/" <> UUID.toText (toUUID aid) <> "/meta"

mkChunkKey :: S3ResumableKey -> S3ChunkNr -> S3ChunkKey
mkChunkKey (S3ResumableKey k) (S3ChunkNr n) =
  S3ChunkKey $ k <> "/" <> nr
  where
    -- Chunk numbers must be between 1 and 10000, as per the S3
    -- multipart upload API, hence the max. left padding of 5 digits.
    nr = Text.justifyRight 5 '0' (Text.pack (show n))

-------------------------------------------------------------------------------
-- S3 Metadata Headers

hAmzMetaUser :: Text
hAmzMetaUser = "user"

hAmzMetaBot :: Text
hAmzMetaBot = "bot"

hAmzMetaProvider :: Text
hAmzMetaProvider = "provider"

hAmzMetaSize :: Text
hAmzMetaSize = "total-size"

hAmzMetaToken :: Text
hAmzMetaToken = "token"

hAmzMetaChunkSize :: Text
hAmzMetaChunkSize = "chunk-size"

hAmzMetaUploadExpires :: Text
hAmzMetaUploadExpires = "upload-expires"

hAmzMetaUploadId :: Text
hAmzMetaUploadId = "upload-id"

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

setAmzMetaTotalSize :: V3.TotalSize -> (Text, Text)
setAmzMetaTotalSize s = (hAmzMetaSize, Text.decodeLatin1 (toByteString' s))

setAmzMetaChunkSize :: V3.ChunkSize -> (Text, Text)
setAmzMetaChunkSize s = (hAmzMetaChunkSize, Text.decodeLatin1 (toByteString' s))

setAmzMetaUploadExpires :: UTCTime -> (Text, Text)
setAmzMetaUploadExpires t = (hAmzMetaUploadExpires, Text.pack (show t))

setAmzMetaUploadId :: Text -> (Text, Text)
setAmzMetaUploadId i = (hAmzMetaUploadId, i)

setAmzMetaPrincipal :: V3.Principal -> (Text, Text)
setAmzMetaPrincipal (V3.UserPrincipal u) = setAmzMetaUser u
setAmzMetaPrincipal (V3.BotPrincipal b) = setAmzMetaBot b
setAmzMetaPrincipal (V3.ProviderPrincipal p) = setAmzMetaProvider p

-------------------------------------------------------------------------------
-- S3 Metadata Getters

lookupCI :: (CI.FoldCase a, Eq a) => a -> [(a, b)] -> Maybe b
lookupCI k = lookup (CI.mk k) . fmap (\(a, b) -> (CI.mk a, b))

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

getAmzMetaUploadExpires :: [(Text, Text)] -> Maybe UTCTime
getAmzMetaUploadExpires h =
  readMay . C8.unpack . encodeUtf8
    =<< lookupCI hAmzMetaUploadExpires h

getAmzMetaTotalSize :: [(Text, Text)] -> Maybe V3.TotalSize
getAmzMetaTotalSize = parseAmzMeta hAmzMetaSize

getAmzMetaChunkSize :: [(Text, Text)] -> Maybe V3.ChunkSize
getAmzMetaChunkSize = parseAmzMeta hAmzMetaChunkSize

getAmzMetaUploadId :: [(Text, Text)] -> Maybe Text
getAmzMetaUploadId = lookupCI hAmzMetaUploadId

parseAmzMeta :: FromByteString a => Text -> [(Text, Text)] -> Maybe a
parseAmzMeta k h = lookupCI k h >>= fromByteString . encodeUtf8

-------------------------------------------------------------------------------
-- Utilities

octets :: MIME.Type
octets = MIME.Type (MIME.Application "octet-stream") []

exec :: (AWSRequest r) => (Text -> r) -> ExceptT Error App (Rs r)
exec req = do
  env <- view aws
  AWS.exec env req

execCatch ::
  (AWSRequest r, Show r) =>
  (Text -> r) ->
  ExceptT Error App (Maybe (Rs r))
execCatch req = do
  env <- view aws
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
  return $ parse <$> HML.toList <$> view horsMetadata <$> r
  where
    req b = headObject (BucketName b) (ObjectKey . Text.pack $ show ast)
    parse =
      maybe False (Text.isInfixOf "public=true" . Text.toLower)
        . lookupCI "zasset"

getOtrMetadata :: ConvId -> AssetId -> ExceptT Error App (Maybe UserId)
getOtrMetadata cnv ast = do
  let S3AssetKey key = otrKey cnv ast
  r <- execCatch (req key)
  return $ getAmzMetaUser =<< HML.toList <$> view horsMetadata <$> r
  where
    req k b = headObject (BucketName b) (ObjectKey k)
