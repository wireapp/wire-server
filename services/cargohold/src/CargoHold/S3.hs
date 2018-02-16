{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module CargoHold.S3
    ( S3AssetKey
    , S3AssetMeta (..)
    , uploadV3
    , getMetadataV3
    , updateMetadataV3
    , deleteV3
    , mkKey
    , signedUrl

      -- * Resumable Uploads
    , S3Resumable
    , resumableOwner
    , resumableTotalSize
    , resumableExpires
    , resumableChunkSize
    , resumableOffset
    , createResumable
    , getResumable
    , completeResumable
    , S3Chunk
    , uploadChunk

      -- Legacy
    , plainKey
    , otrKey
    , getMetadata
    , getOtrMetadata
    ) where

import Aws.Core (ResponseConsumer (..), SignQuery (..))
import Aws.S3
import CargoHold.App hiding (Handler)
import Control.Applicative ((<|>))
import CargoHold.API.Error
import Control.Error (ExceptT, throwE)
import Control.Lens hiding ((.=), (:<), (:>), parts)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Retry
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Conduit
import Data.Foldable (toList)
import Data.Id
import Data.List (foldl')
import Data.Maybe
import Data.Monoid ((<>))
import Data.Sequence (Seq, ViewR (..), ViewL (..))
import Data.Time.Clock
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8) --, decodeLatin1)
import Network.HTTP.Types.URI (urlEncode)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error (Error (..))
import Safe (readMay)
import System.Logger.Message
import URI.ByteString hiding (urlEncode)

import qualified Aws.Core                     as Aws
import qualified Bilge.Retry                  as Retry
import qualified CargoHold.AWS                as AWS
import qualified CargoHold.Types.V3           as V3
import qualified CargoHold.Types.V3.Resumable as V3
import qualified Codec.MIME.Type              as MIME
import qualified Codec.MIME.Parse             as MIME
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.CaseInsensitive         as CI
import qualified Data.Conduit                 as Conduit
import qualified Data.Conduit.Binary          as Conduit
import qualified Data.List.NonEmpty           as NE
import qualified Data.Sequence                as Seq
import qualified Data.HashMap.Lazy            as HML
import qualified Data.Text                    as Text
import qualified Data.Text.Ascii              as Ascii
import qualified Data.Text.Encoding           as Text
import qualified Data.UUID                    as UUID
import qualified Network.AWS                  as NAWS
import qualified Network.AWS.S3               as AWS
import qualified Network.AWS.Data.Body        as AWS
import qualified Network.AWS.Data.Crypto      as AWS
import qualified Ropes.Aws                    as Aws
import qualified System.Logger.Class          as Log

newtype S3AssetKey = S3AssetKey { s3Key :: Text }
    deriving (Eq, Show, ToByteString)

-- | Asset metadata tracked in S3.
data S3AssetMeta = S3AssetMeta
    { v3AssetOwner :: V3.Principal
    , v3AssetToken :: Maybe V3.AssetToken
    , v3AssetType  :: MIME.Type
    } deriving Show

uploadV3 :: V3.Principal
         -> V3.AssetKey
         -> V3.AssetHeaders
         -> Maybe V3.AssetToken
         -> Source (ResourceT IO) ByteString
         -> ExceptT Error App ()
uploadV3 prc (s3Key . mkKey -> key) (V3.AssetHeaders ct cl md5) tok src = do
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.owner" .= toByteString prc
        ~~ "asset.key"   .= key
        ~~ "asset.type"  .= MIME.showType ct
        ~~ "asset.size"  .= cl
        ~~ msg (val "Uploading asset")
        ~~ msg (val "MD5: ")
        ~~ msg (show md5)
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    void $ AWS.execute e (AWS.send $ req b key)
  where
    req b k =
        let rqBody = AWS.ChunkedBody AWS.defaultChunkSize (fromIntegral cl) src
            md5Res = Text.decodeLatin1 $ AWS.digestToBase AWS.Base64 md5
         in   AWS.putObject (AWS.BucketName b) (AWS.ObjectKey k) (AWS.toBody rqBody)
            & AWS.poContentType ?~ encodeMIMEType' ct
            & AWS.poContentMD5  ?~ md5Res
            & AWS.poMetadata    .~ (HML.fromList
                                   $ catMaybes [ setAmzMetaToken <$> tok
                                               , Just (setAmzMetaPrincipal prc)
                                               ]
                                   )

getMetadataV3 :: V3.AssetKey -> ExceptT Error App (Maybe S3AssetMeta)
getMetadataV3 (s3Key . mkKey -> key) = do
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    r' <- AWS.execute e (ho b key)
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.key" .= key
        ~~ "asset.bucket" .= b
        ~~ msg (val "Getting asset metadata")
        ~~ msg (show r')
    case r' of
        Nothing -> return Nothing
        Just r  -> do 
            let ct = fromMaybe octets (parseMIMEType =<< view AWS.horsContentType r)
            let meta = HML.toList $ view AWS.horsMetadata r
            return $ parse ct meta
  where
    ho :: Text -> Text -> AWS.Amazon (Maybe AWS.HeadObjectResponse)
    ho b k = do
        let req = AWS.headObject (AWS.BucketName b) (AWS.ObjectKey k)
        resp <- retrying AWS.retry5x (const AWS.canRetry) (const (AWS.sendCatch req))
        return $ either (const Nothing) Just resp

    parse ct h = S3AssetMeta
        <$> getAmzMetaPrincipal h
        <*> Just (getAmzMetaToken h)
        <*> Just ct

deleteV3 :: V3.AssetKey -> ExceptT Error App ()
deleteV3 (s3Key . mkKey -> key) = do
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.key" .= key
        ~~ msg (val "Deleting asset")
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    let req = AWS.deleteObject (AWS.BucketName b) (AWS.ObjectKey key)
    void $ AWS.execute e (AWS.send req)

updateMetadataV3 :: V3.AssetKey -> S3AssetMeta -> ExceptT Error App ()
updateMetadataV3 (s3Key . mkKey -> key) (S3AssetMeta prc tok ct) = do
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    let hdrs = HML.fromList $ catMaybes [ setAmzMetaToken <$> tok
                                        , Just (setAmzMetaPrincipal prc)
                                        ]
    let req = AWS.copyObject (AWS.BucketName b) (copySrc b key) (AWS.ObjectKey key)
            & AWS.coContentType ?~ encodeMIMEType' ct
            & AWS.coMetadataDirective ?~ AWS.MDReplace
            & AWS.coMetadata .~ hdrs
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.owner" .= show prc
        ~~ "asset.key"   .= key
        ~~ msg (val "Updating asset metadata")
    void $ AWS.execute e (AWS.send req)
  where
    copySrc b k = Text.decodeLatin1 $ urlEncode True $ Text.encodeUtf8 (b <> "/" <> k)

signedUrl :: V3.AssetKey -> ExceptT Error App URI
signedUrl (s3Key . mkKey -> key) = do
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    now <- liftIO getCurrentTime
    let expiresIn = NAWS.Seconds 300
    let req = AWS.getObject (AWS.BucketName b) (AWS.ObjectKey key)
    signed <- AWS.execute e (NAWS.presignURL now expiresIn req)
    return =<< toUri signed
  where
    toUri x = case parseURI strictURIParserOptions x of
        Left _  -> throwE invalidURI
        Right u -> return u

mkKey :: V3.AssetKey -> S3AssetKey
mkKey (V3.AssetKeyV3 i r) = S3AssetKey $ "v3/" <> retention <> "/" <> key
  where
    key = UUID.toText (toUUID i)
    retention = case r of
        V3.AssetEternal    -> "eternal"
        V3.AssetPersistent -> "persistent"
        V3.AssetVolatile   -> "volatile"

metaHeaders :: Maybe V3.AssetToken -> V3.Principal -> HML.HashMap Text Text
metaHeaders tok prc = HML.fromList
                    $ catMaybes [ setAmzMetaToken <$> tok
                                , Just (setAmzMetaPrincipal prc)
                                ]

-------------------------------------------------------------------------------
-- Resumable Uploads

newtype S3ResumableKey = S3ResumableKey { s3ResumableKey :: Text }
    deriving (Eq, Show, ToByteString)

newtype S3ChunkKey = S3ChunkKey { s3ChunkKey :: Text }
    deriving (Eq, Show, ToByteString)

newtype S3ChunkNr = S3ChunkNr Word
    deriving (Eq, Ord, Show, ToByteString, FromByteString, Num, Integral, Enum, Real)

newtype S3ETag = S3ETag { s3ETag :: Text }
    deriving (Eq, Show, ToByteString, FromByteString)

data S3Resumable = S3Resumable
    { resumableKey       :: S3ResumableKey      -- ^ The resumable asset key.
    , resumableAsset     :: V3.AssetKey         -- ^ The final asset key.
    , resumableOwner     :: V3.Principal        -- ^ The creator (i.e. owner).
    , resumableChunkSize :: V3.ChunkSize        -- ^ Size of each chunk.
    , resumableTotalSize :: V3.TotalSize        -- ^ Size of the final asset.
    , resumableType      :: MIME.Type           -- ^ MIME type of the final asset.
    , resumableToken     :: Maybe V3.AssetToken -- ^ Token of the final asset.
    , resumableExpires   :: UTCTime             -- ^ Expiry of the resumable upload.
    , resumableUploadId  :: Maybe Text          -- ^ S3 multipart upload ID, if any.
    , resumableChunks    :: Seq S3Chunk
    } deriving (Show)

data S3Chunk = S3Chunk
    { chunkNr     :: S3ChunkNr -- ^ Sequence nr.
    , chunkOffset :: V3.Offset -- ^ Offset of the first byte.
    , chunkSize   :: Word      -- ^ (Actual) Size of the chunk.
    , chunkETag   :: S3ETag    -- ^ S3 ETag.
    } deriving (Show)

mkChunkNr :: S3Resumable -> V3.Offset -> S3ChunkNr
mkChunkNr r o = S3ChunkNr ((offBytes `quot` chunkBytes) + 1)
  where
    offBytes   = V3.offsetBytes o
    chunkBytes = V3.chunkSizeBytes (resumableChunkSize r)

mkOffset :: S3Resumable -> S3ChunkNr -> V3.Offset
mkOffset r n = V3.Offset ((fromIntegral n - 1) * chunkBytes)
  where
    chunkBytes = V3.chunkSizeBytes (resumableChunkSize r)

resumableOffset :: S3Resumable -> V3.Offset
resumableOffset r = case Seq.viewr (resumableChunks r) of
    Seq.EmptyR -> V3.Offset 0
    _   :>   c -> chunkOffset c + V3.Offset (chunkSize c)

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
        bigChunks   = max 1 (min maxTotalChunks (total `quot` minBigSize))
        smallSize   = total `quot` smallChunks
        bigSize     = total `quot` bigChunks
    in V3.ChunkSize $
        if | smallChunks <  maxSmallChunks -> minSmallSize
           | smallSize   <= maxSmallSize   -> smallSize
           | bigChunks   <  maxTotalChunks -> minBigSize
           | otherwise                     -> bigSize

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
    let rk = mkResumableKey k
    Log.debug $ "remote" .= val "S3"
        ~~ "asset"       .= toByteString k
        ~~ "asset.key"   .= toByteString rk
        ~~ msg (val "Getting resumable asset metadata")
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    let req = AWS.headObject (AWS.BucketName b) (AWS.ObjectKey $ s3ResumableKey rk)
    resp <- AWS.execute e (retrying AWS.retry5x (const AWS.canRetry) (const (AWS.sendCatch req)))
    case resp of
        Left _  -> return Nothing
        Right r -> do
            let ct = fromMaybe octets (parseMIMEType =<< view AWS.horsContentType r)
            let meta = HML.toList $ view AWS.horsMetadata r
            case parse rk ct meta of
                Nothing -> return Nothing
                Just r' -> fmap (\cs -> r' { resumableChunks = cs }) <$> listChunks r'
  where
    parse rk ct h = S3Resumable rk k
        <$> getAmzMetaPrincipal h
        <*> getAmzMetaChunkSize h
        <*> getAmzMetaTotalSize h
        <*> pure ct
        <*> Just (getAmzMetaToken h)
        <*> getAmzMetaUploadExpires h
        <*> Just (getAmzMetaUploadId h)
        <*> pure Seq.empty

createResumable
    :: V3.AssetKey
    -> V3.Principal
    -> MIME.Type
    -> V3.TotalSize
    -> Maybe V3.AssetToken
    -> ExceptT Error App S3Resumable
createResumable k p typ size tok = do
    let csize = calculateChunkSize size
    ex <- addUTCTime V3.assetVolatileSeconds <$> liftIO getCurrentTime
    let key = mkResumableKey k
    let res = S3Resumable key k p csize size typ tok ex Nothing Seq.empty
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    up <- initMultipart e b res
    let ct = resumableType res
    void $ AWS.execute e (AWS.send $ first b (s3ResumableKey key) ct (resumableMeta csize ex up))
    return res { resumableUploadId = up }
  where
    initMultipart e b r
        | canUseMultipart r = do
            let cmu = AWS.createMultipartUpload (AWS.BucketName b) (AWS.ObjectKey $ s3Key (mkKey k))
                    & AWS.cmuContentType ?~ MIME.showType (resumableType r)
                    & AWS.cmuMetadata    .~ metaHeaders (resumableToken r) p
            imur <- AWS.execute e (AWS.send cmu)
            return $! view AWS.cmursUploadId imur
        | otherwise = return Nothing

    first b key ct meta =
          AWS.putObject (AWS.BucketName b) (AWS.ObjectKey key) (AWS.toBody (mempty :: ByteString))
        & AWS.poContentType ?~ encodeMIMEType' ct
        & AWS.poMetadata    .~ HML.fromList meta

    -- Determine whether a given 'S3Resumable' is eligible for the
    -- S3 multipart upload API. That is the case if the chunk size
    -- is >= 5 MiB or if there is only 1 chunk (<= 'minSmallSize').
    canUseMultipart r = chunkBytes >= minBigSize || totalBytes <= minSmallSize
      where
        chunkBytes = V3.chunkSizeBytes (resumableChunkSize r)
        totalBytes = V3.totalSizeBytes (resumableTotalSize r)

    resumableMeta csize expires upl =
          setAmzMetaPrincipal p
        : setAmzMetaTotalSize size
        : setAmzMetaChunkSize csize
        : setAmzMetaUploadExpires expires
        : catMaybes
        [ setAmzMetaToken <$> tok
        , setAmzMetaUploadId <$> upl
        ]

uploadChunk
    :: S3Resumable
    -> V3.Offset
    -> ResumableSource IO ByteString
    -> ExceptT Error App (S3Resumable, ResumableSource IO ByteString)
uploadChunk r offset rsrc = do
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    let chunkSize = fromIntegral (resumableChunkSize r)
    (rest, chunk) <- liftIO $ rsrc $$++ Conduit.take chunkSize
    let size = fromIntegral (LBS.length chunk)
    Log.debug $ "remote"  .= val "S3"
        ~~ "asset"        .= toByteString (resumableAsset r)
        ~~ "asset.owner"  .= toByteString (resumableOwner r)
        ~~ "asset.key"    .= toByteString (resumableKey r)
        ~~ "asset.chunk"  .= toByteString nr
        ~~ "asset.offset" .= toByteString offset
        ~~ "asset.size"   .= toByteString size
        ~~ msg (val "Uploading chunk")
    c <- case resumableUploadId r of
        Nothing -> putChunk e b chunk size
        Just up -> putPart e b up chunk size
    let r' = r { resumableChunks = resumableChunks r Seq.|> c }
    return (r', rest)
  where
    nr = mkChunkNr r offset
    ct = encodeMIMEType (resumableType r)

    putChunk e b chunk size = do
        let S3ChunkKey k = mkChunkKey (resumableKey r) nr
        let req = AWS.putObject (AWS.BucketName b) (AWS.ObjectKey k) (AWS.toBody chunk)
                & AWS.poContentType ?~ Text.decodeLatin1 ct
        void $ AWS.execute e (AWS.send req)
        return $! S3Chunk nr offset size (S3ETag "")

    putPart e b up chunk size = do
        let S3AssetKey k = mkKey (resumableAsset r)
        -- TODO: Really, no content-type ?
        let req = AWS.uploadPart (AWS.BucketName b)
                                 (AWS.ObjectKey k)
                                 (fromIntegral nr)
                                 up
                                 (AWS.toBody chunk)
        upr  <- AWS.execute e (AWS.send req)
        etag <- case view AWS.uprsETag upr of
            Just (AWS.ETag tag) -> return $ S3ETag (Text.decodeLatin1 tag)
            Nothing             -> throwE serverError
        return $! S3Chunk nr offset size etag

-- | Complete a resumable upload, assembling all chunks into a final asset.
completeResumable :: S3Resumable -> ExceptT Error App ()
completeResumable r = do
    let own = resumableOwner r
    let ast = resumableAsset r
    Log.debug $ "remote" .= val "S3"
        ~~ "asset"       .= toByteString ast
        ~~ "asset.owner" .= toByteString own
        ~~ "asset.key"   .= toByteString (resumableKey r)
        ~~ msg (val "Completing resumable upload")
    let chunks = resumableChunks r
    verifyChunks chunks
    e <- view awsAmazonka
    let b = view AWS.s3Bucket e
    case resumableUploadId r of
        Nothing -> assembleLocal e b chunks
        Just up -> assembleRemote e b up (NE.nonEmpty $ toList chunks)
    Log.debug $ "remote" .= val "S3"
        ~~ "asset"       .= toByteString ast
        ~~ "asset.owner" .= toByteString own
        ~~ "asset.key"   .= toByteString (resumableKey r)
        ~~ msg (val "Resumable upload completed")
  where
    -- Local assembly for small chunk sizes (< 5 MiB): Download and re-upload
    -- the chunks in a streaming fashion one-by-one to create the final object.
    assembleLocal e b chunks = do
        env <- awsEnv   <$> view aws
        s3c <- s3Config <$> view aws
        let own = resumableOwner r
        let ast = resumableAsset r
        let size = resumableTotalSize r
        let rqBody = AWS.Chunked $ AWS.ChunkedBody AWS.defaultChunkSize (fromIntegral size) (chunkSource e env s3c b chunks)
        let putRq = AWS.putObject (AWS.BucketName b) (AWS.ObjectKey (s3Key (mkKey ast))) rqBody
                & AWS.poContentType ?~ encodeMIMEType' (resumableType r)
                & AWS.poMetadata    .~ metaHeaders (resumableToken r) own
        void $ AWS.execute e (AWS.send putRq)

        -- For symmetry with the behavior of the S3 multipart API, where the
        -- resumable upload and all parts are removed upon completion, we do
        -- the same here.
        let rk = resumableKey r
        let keys = s3ResumableKey rk
                 : map (s3ChunkKey . mkChunkKey rk . chunkNr) (toList chunks)
        let del = AWS.delete' & AWS.dQuiet ?~ True
                              & AWS.dObjects .~ map (AWS.objectIdentifier . AWS.ObjectKey) keys
        let delRq = AWS.deleteObjects (AWS.BucketName b) del
        void $ AWS.execute e (AWS.send delRq)

    -- Remote assembly for large(r) chunk sizes (>= 5 MiB) via the
    -- S3 multipart upload API.
    assembleRemote _ _ _  Nothing       = throwE serverError
    assembleRemote e b up (Just chunks) = do
        let ast = resumableAsset r
        let key = s3Key (mkKey ast)
        let parts = fmap mkPart chunks
        let completeRq = AWS.completeMultipartUpload (AWS.BucketName b) (AWS.ObjectKey key) up
                       & AWS.cMultipartUpload ?~ (AWS.completedMultipartUpload & (AWS.cmuParts ?~ parts))
        void $ AWS.execute e (AWS.send completeRq)
        let S3ResumableKey rkey = resumableKey r
        let delRq = AWS.deleteObject (AWS.BucketName b) (AWS.ObjectKey rkey)
        void $ AWS.execute e (AWS.send delRq)

    mkPart c = AWS.completedPart (fromIntegral (chunkNr c)) (AWS.ETag . Text.encodeUtf8 $ s3ETag (chunkETag c))

    -- Verify that the chunks constitute the full asset, i.e. that the
    -- upload is complete.
    verifyChunks cs = do
        let !total = V3.TotalSize $ foldl' (\t v -> t + chunkSize v) 0 cs
        unless (total == resumableTotalSize r) $
            throwE $ uploadIncomplete (resumableTotalSize r) total

    -- Construct a 'Source' by downloading the chunks.
    chunkSource :: AWS.Env
                -> Aws.Env
                -> S3Configuration Aws.NormalQuery
                -> Bucket
                -> Seq S3Chunk
                -> Source (ResourceT IO) ByteString
    chunkSource env oldEnv s3c b cs = case Seq.viewl cs of
        EmptyL  -> mempty
        c :< cc -> do
            let S3ChunkKey ck = mkChunkKey (resumableKey r) (chunkNr c)
            let req = AWS.getObject (AWS.BucketName b) (AWS.ObjectKey ck)
            v <- lift $ AWS.execute env $ AWS.send req
                      >>= flip NAWS.sinkBody Conduit.sinkLbs . view AWS.gorsBody
            Conduit.yield (LBS.toStrict v) >> chunkSource env oldEnv s3c b cc

listChunks :: S3Resumable -> ExceptT Error App (Maybe (Seq S3Chunk))
listChunks r = do
    e <- view awsAmazonka
    let ast = resumableAsset r
    let S3ResumableKey key = resumableKey r
    Log.debug $ "remote" .= val "S3"
        ~~ "asset" .= toByteString ast
        ~~ "asset.resumable" .= key
        ~~ msg (val "Listing chunks")
    b <- s3Bucket <$> view aws
    fmap Seq.fromList <$> case resumableUploadId r of
        Nothing -> listBucket e b key
        Just up -> listParts e b up
  where
    listBucket e b k = do
        let req = AWS.listObjects (AWS.BucketName b)
                & AWS.loPrefix  ?~ (k <> "/")
                & AWS.loMaxKeys ?~ fromIntegral maxTotalChunks
        resp <- AWS.execute e (retrying AWS.retry5x (const AWS.canRetry) (const (AWS.sendCatch req)))
        case resp of
            Left _    -> return Nothing
            Right lor -> return . Just $ mapMaybe chunkFromObject (view AWS.lorsContents lor)

    listParts e b up = do
        let req = AWS.listParts (AWS.BucketName b)
                                (AWS.ObjectKey $ s3Key (mkKey (resumableAsset r)))
                                up
        resp <- AWS.execute e (retrying AWS.retry5x (const AWS.canRetry) (const (AWS.sendCatch req)))
        case resp of
            Left _    -> return Nothing
            Right lpr -> return . Just
                                $ fmap chunkFromPart
                                . catMaybes
                                . fmap toAWSPart $ (view AWS.lprsParts lpr)

    toAWSPart :: AWS.Part -> Maybe PartInfo
    toAWSPart p = case (view AWS.pPartNumber p, view AWS.pETag p, view AWS.pSize p) of
        (Just x, Just (AWS.ETag y), Just z) -> Just $ PartInfo (fromIntegral x) (Text.decodeLatin1 y) (fromIntegral z)
        _                        -> Nothing

    chunkFromObject :: AWS.Object -> Maybe S3Chunk
    chunkFromObject o = do
        let (AWS.ObjectKey okey) = (view AWS.oKey o)
        nr <- parseNr okey
        let etag = let (AWS.ETag t) = (view AWS.oETag o)
                    in S3ETag (Text.decodeLatin1 t)
        let size = fromIntegral (view AWS.oSize o)
        let off  = mkOffset r nr
        Just $! S3Chunk nr off size etag

    chunkFromPart p =
        let nr   = S3ChunkNr (piNr p)
            off  = mkOffset r nr
            size = piSize p
            etag = S3ETag (piETag p)
        in S3Chunk nr off size etag

    parseNr = fmap S3ChunkNr . readMay . Text.unpack . snd . Text.breakOnEnd "/"

mkResumableKey :: V3.AssetKey -> S3ResumableKey
mkResumableKey (V3.AssetKeyV3 aid _) =
    S3ResumableKey $ "v3/resumable/" <> UUID.toText (toUUID aid)

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
setAmzMetaPrincipal (V3.UserPrincipal     u) = setAmzMetaUser     u
setAmzMetaPrincipal (V3.BotPrincipal      b) = setAmzMetaBot      b
setAmzMetaPrincipal (V3.ProviderPrincipal p) = setAmzMetaProvider p

-------------------------------------------------------------------------------
-- S3 Metadata Getters

lookupCI :: (CI.FoldCase a, Eq a) => a -> [(a, b)] -> Maybe b
lookupCI k = lookup (CI.mk k) . fmap (\(a,b) -> (CI.mk a, b))

getAmzMetaPrincipal :: [(Text, Text)] -> Maybe V3.Principal
getAmzMetaPrincipal h =
    (V3.UserPrincipal     <$> getAmzMetaUser     h) <|>
    (V3.BotPrincipal      <$> getAmzMetaBot      h) <|>
    (V3.ProviderPrincipal <$> getAmzMetaProvider h)

getAmzMetaUser :: [(Text, Text)] -> Maybe UserId
getAmzMetaUser = parseAmzMeta hAmzMetaUser

getAmzMetaBot :: [(Text, Text)] -> Maybe BotId
getAmzMetaBot = parseAmzMeta hAmzMetaBot

getAmzMetaProvider :: [(Text, Text)] -> Maybe ProviderId
getAmzMetaProvider = parseAmzMeta hAmzMetaProvider

getAmzMetaToken :: [(Text, Text)] -> Maybe V3.AssetToken
getAmzMetaToken h = V3.AssetToken . Ascii.unsafeFromText
                 <$> lookupCI hAmzMetaToken h

getAmzMetaUploadExpires :: [(Text, Text)] -> Maybe UTCTime
getAmzMetaUploadExpires h = readMay . C8.unpack . encodeUtf8
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

tryS3 :: ResourceT App a -> ExceptT Error App a
tryS3 action = do
    e <- lift ask
    runAppResourceT e action `catch`
        \(ex :: S3Error) -> case s3ErrorCode ex of
            "InvalidDigest"  -> throwE invalidMD5
            "BadDigest"      -> throwE invalidMD5
            "RequestTimeout" -> throwE requestTimeout
            _                -> do
                lift . Log.err $ msg (show ex)
                throwE serverError

exec :: (Aws.Transaction r a, ServiceConfiguration r ~ S3Configuration)
     => r -> ResourceT App (ResponseMetadata a, a)
exec req = do
    env <- awsEnv   <$> view aws
    s3c <- s3Config <$> view aws
    Aws.sendRequest env s3c req

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 200000

handlers :: Monad m => [RetryStatus -> Handler m Bool]
handlers = Retry.httpHandlers ++
    [ const $ Handler $ \(S3Error s _ _ _ _ _ _ _ _ _) ->
        return $ statusIsServerError s
    ]

parseMIMEType :: Text -> Maybe MIME.Type
parseMIMEType = MIME.parseMIMEType

encodeMIMEType :: MIME.Type -> ByteString
encodeMIMEType = Text.encodeUtf8 . MIME.showType

encodeMIMEType' :: MIME.Type -> Text
encodeMIMEType' = MIME.showType

octets :: MIME.Type
octets = MIME.Type (MIME.Application "octet-stream") []

--------------------------------------------------------------------------------

data PartInfo = PartInfo
    { piNr   :: Word
    , piETag :: Text
    , piSize :: Word
    }

--------------------------------------------------------------------------------
-- Legacy

plainKey :: AssetId -> S3AssetKey
plainKey a = S3AssetKey $ Text.pack (show a)

otrKey :: ConvId -> AssetId -> S3AssetKey
otrKey c a = S3AssetKey $ "otr/" <> Text.pack (show c) <> "/" <> Text.pack (show a)

getMetadata :: AssetId -> ExceptT Error App (Maybe Bool)
getMetadata aId = do
    b <- s3Bucket <$> view aws
    (_, r) <- tryS3 . recovering x3 handlers . const . exec $
                  headObject b (Text.pack $ show aId)
    return $ parse <$> (omUserMetadata <$> horMetadata r)
  where
    parse = maybe False (Text.isInfixOf "public=true" . Text.toLower)
          . lookupCI "zasset"

getOtrMetadata :: ConvId -> AssetId -> ExceptT Error App (Maybe UserId)
getOtrMetadata cnv ast = do
    b <- s3Bucket <$> view aws
    let S3AssetKey key = otrKey cnv ast
    (_, r) <- tryS3 . recovering x3 handlers . const $ exec (headObject b key)
    return $ (omUserMetadata <$> horMetadata r) >>= getAmzMetaUser

