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
    , signedURL

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

import Aws.Core (ResponseConsumer (..), SignQuery (..), throwStatusCodeException)
import Aws.S3
import CargoHold.App hiding (Handler)
import Control.Applicative ((<|>))
import CargoHold.API.Error
import CargoHold.Options
import Control.Error (ExceptT, throwE)
import Control.Lens (view)
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
import Data.Text.Encoding (encodeUtf8, decodeLatin1)
import Network.HTTP.Client.Conduit (requestBodySource)
import Network.HTTP.Client
import Network.HTTP.Types (toQuery)
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error (Error (..))
import Safe (readMay)
import System.Logger.Message
import Text.XML.Cursor (laxElement, ($/), (&|))
import URI.ByteString

import qualified Aws                          as Aws
import qualified Aws.Core                     as Aws
import qualified Bilge.Retry                  as Retry
import qualified CargoHold.Types.V3           as V3
import qualified CargoHold.Types.V3.Resumable as V3
import qualified Codec.MIME.Type              as MIME
import qualified Codec.MIME.Parse             as MIME
import qualified Data.ByteString.Char8        as C8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Conduit                 as Conduit
import qualified Data.Conduit.Binary          as Conduit
import qualified Data.Sequence                as Seq
import qualified Data.Text                    as Text
import qualified Data.Text.Ascii              as Ascii
import qualified Data.Text.Encoding           as Text
import qualified Data.UUID                    as UUID
import qualified Network.HTTP.Conduit         as Http
import qualified Ropes.Aws                    as Aws
import qualified System.Logger.Class          as Log

newtype S3AssetKey = S3AssetKey { s3Key :: Text }
    deriving (Eq, Show, ToByteString)

-- | Asset metadata tracked in S3.
data S3AssetMeta = S3AssetMeta
    { v3AssetOwner :: V3.Principal
    , v3AssetToken :: Maybe V3.AssetToken
    , v3AssetType  :: MIME.Type
    }

uploadV3 :: V3.Principal
         -> V3.AssetKey
         -> V3.AssetHeaders
         -> Maybe V3.AssetToken
         -> Conduit.Source IO ByteString
         -> ExceptT Error App ()
uploadV3 prc (s3Key . mkKey -> key) (V3.AssetHeaders ct cl md5) tok src = do
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.owner" .= toByteString prc
        ~~ "asset.key"   .= key
        ~~ "asset.type"  .= MIME.showType ct
        ~~ "asset.size"  .= cl
        ~~ msg (val "Uploading asset")
    b <- s3Bucket <$> view aws
    let body = requestBodySource (fromIntegral cl) src
    void . tryS3 . exec $ (putObject b key body)
        { poContentMD5        = Just md5
        , poContentType       = Just (encodeMIMEType ct)
        , poExpect100Continue = True
        , poMetadata          = catMaybes
                              [ setAmzMetaToken <$> tok
                              , Just (setAmzMetaPrincipal prc)
                              ]
        }

getMetadataV3 :: V3.AssetKey -> ExceptT Error App (Maybe S3AssetMeta)
getMetadataV3 (s3Key . mkKey -> key) = do
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.key" .= key
        ~~ msg (val "Getting asset metadata")
    b <- s3Bucket <$> view aws
    (_, r) <- tryS3 . recovering x3 handlers . const . exec $ headObjectX b key
    let ct = fromMaybe octets (parseMIMEType =<< horxContentType r)
    return $ parse ct =<< (omUserMetadata <$> horxMetadata r)
  where
    parse ct h = S3AssetMeta
        <$> getAmzMetaPrincipal h
        <*> Just (getAmzMetaToken h)
        <*> Just ct

deleteV3 :: V3.AssetKey -> ExceptT Error App ()
deleteV3 (s3Key . mkKey -> key) = do
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.key" .= key
        ~~ msg (val "Deleting asset")
    b <- s3Bucket <$> view aws
    void . tryS3 $ exec (DeleteObject key b)

updateMetadataV3 :: V3.AssetKey -> S3AssetMeta ->  ExceptT Error App ()
updateMetadataV3 (s3Key . mkKey -> key) (S3AssetMeta prc tok ct) = do
    Log.debug $ "remote" .= val "S3"
        ~~ "asset.owner" .= show prc
        ~~ "asset.key"   .= key
        ~~ msg (val "Updating asset metadata")
    b <- s3Bucket <$> view aws
    let hdrs = catMaybes
             [ setAmzMetaToken <$> tok
             , Just (setAmzMetaPrincipal prc)
             ]
    let meta = ReplaceMetadata hdrs
    void . tryS3 . recovering x3 handlers . const . exec $
        (copyObject b key (ObjectId b key Nothing) meta)
            { coContentType = Just (encodeMIMEType ct) }

signedURL :: (ToByteString p) => p -> ExceptT Error App URI
signedURL path = do
    e <- view aws
    b <- s3Bucket <$> view aws
    cfg' <- liftIO $ Aws.getConfig (awsEnv e)
    ttl  <- view (settings.setDownloadLinkTTL)
    let cfg = cfg' { Aws.timeInfo = Aws.ExpiresIn (fromIntegral ttl) }
    uri <- liftIO $ Aws.awsUri cfg (s3UriOnly e)
                  $ getObject b (Text.decodeLatin1 $ toByteString' path)
    return =<< toUri uri
  where
    toUri x = case parseURI strictURIParserOptions x of
        Left e  -> do
            Log.err $ "remote" .= val "S3"
                    ~~ msg (val "Failed to generate a signed URI")
                    ~~ msg (show e)
            throwE serverError
        Right u -> return u

mkKey :: V3.AssetKey -> S3AssetKey
mkKey (V3.AssetKeyV3 i r) = S3AssetKey $ "v3/" <> retention <> "/" <> key
  where
    key = UUID.toText (toUUID i)
    retention = V3.retentionToTextRep r

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
    b      <- s3Bucket <$> view aws
    (_, hor) <- tryS3 . recovering x3 handlers . const . exec $
        headObjectX b (s3ResumableKey rk)
    let ct = fromMaybe octets (horxContentType hor >>= parseMIMEType)
    let meta = omUserMetadata <$> horxMetadata hor
    case meta >>= parse rk ct of
        Nothing -> return Nothing
        Just  r -> fmap (\cs -> r { resumableChunks = cs }) <$> listChunks r
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
    b  <- s3Bucket <$> view aws
    up <- initMultipart b res
    let ct = resumableType res
    void . tryS3 . recovering x3 handlers . const . exec $
        (putObject b (s3ResumableKey key) mempty)
            { poContentType = Just (encodeMIMEType ct)
            , poMetadata = resumableMeta csize ex up
            }
    return res { resumableUploadId = up }
  where
    initMultipart b r
        | canUseMultipart r = do
            (_, imur) <- tryS3 . recovering x3 handlers . const . exec $
                (postInitiateMultipartUpload b (s3Key (mkKey k)))
                    { imuContentType = Just (MIME.showType (resumableType r))
                    , imuMetadata = catMaybes
                        [ setAmzMetaToken <$> resumableToken r
                        , Just (setAmzMetaPrincipal p)
                        ]
                    }
            return $! Just $! imurUploadId imur
        | otherwise = return Nothing

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
    b <- s3Bucket <$> view aws
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
        Nothing -> putChunk b chunk size
        Just up -> putPart b up chunk size
    let r' = r { resumableChunks = resumableChunks r Seq.|> c }
    return (r', rest)
  where
    nr = mkChunkNr r offset
    ct = encodeMIMEType (resumableType r)

    putChunk b chunk size = do
        let S3ChunkKey k = mkChunkKey (resumableKey r) nr
        void . tryS3 . recovering x3 handlers . const . exec $
            (putObject b k (RequestBodyLBS chunk))
                { poContentType = Just ct
                , poExpect100Continue = True
                }
        return $! S3Chunk nr offset size (S3ETag "")

    putPart b up chunk size = do
        let S3AssetKey k = mkKey (resumableAsset r)
        (_, upr) <- tryS3 . recovering x3 handlers . const . exec $
            (uploadPart b k (fromIntegral nr) up (RequestBodyLBS chunk))
                { upContentType = Just ct
                , upExpect100Continue = True
                , upContentMD5 = Nothing
                }
        let etag = S3ETag (uprETag upr)
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
    b <- s3Bucket <$> view aws
    case resumableUploadId r of
        Nothing -> assembleLocal b chunks
        Just up -> assembleRemote b up chunks
    Log.debug $ "remote" .= val "S3"
        ~~ "asset"       .= toByteString ast
        ~~ "asset.owner" .= toByteString own
        ~~ "asset.key"   .= toByteString (resumableKey r)
        ~~ msg (val "Resumable upload completed")
  where
    -- Local assembly for small chunk sizes (< 5 MiB): Download and re-upload
    -- the chunks in a streaming fashion one-by-one to create the final object.
    assembleLocal b chunks = do
        env <- awsEnv   <$> view aws
        s3c <- s3Config <$> view aws
        let own = resumableOwner r
        let ast = resumableAsset r
        let size = fromIntegral (resumableTotalSize r)
        let body = Http.requestBodySource size (chunkSource env s3c b chunks)
        void . tryS3 . exec $
            (putObject b (s3Key (mkKey ast)) body)
                { poContentType       = Just (encodeMIMEType (resumableType r))
                , poExpect100Continue = True
                , poMetadata          = catMaybes
                    [ setAmzMetaToken <$> resumableToken r
                    , Just (setAmzMetaPrincipal own)
                    ]
                }
        -- For symmetry with the behavior of the S3 multipart API, where the
        -- resumable upload and all parts are removed upon completion, we do
        -- the same here.
        let rk = resumableKey r
        let keys = s3ResumableKey rk
                 : map (s3ChunkKey . mkChunkKey rk . chunkNr) (toList chunks)
        void . tryS3 . exec $ (deleteObjects b keys) { dosQuiet = True }

    -- Remote assembly for large(r) chunk sizes (>= 5 MiB) via the
    -- S3 multipart upload API.
    assembleRemote b up chunks = do
        let ast = resumableAsset r
        let key = s3Key (mkKey ast)
        let parts = map mkPart (toList chunks)
        void . tryS3 . exec $ postCompleteMultipartUpload b key up parts
        let S3ResumableKey rkey = resumableKey r
        void . tryS3 . exec $ DeleteObject
            { doBucket     = b
            , doObjectName = rkey
            }

    mkPart c = (fromIntegral (chunkNr c), s3ETag (chunkETag c))

    -- Verify that the chunks constitute the full asset, i.e. that the
    -- upload is complete.
    verifyChunks cs = do
        let !total = V3.TotalSize $ foldl' (\t v -> t + chunkSize v) 0 cs
        unless (total == resumableTotalSize r) $
            throwE $ uploadIncomplete (resumableTotalSize r) total

    -- Construct a 'Source' by downloading the chunks.
    chunkSource env s3c b cs = case Seq.viewl cs of
        EmptyL  -> mempty
        c :< cc -> do
            (src, fin) <- lift $ do
                let S3ChunkKey ck = mkChunkKey (resumableKey r) (chunkNr c)
                (_, gor) <- recovering x3 handlers $ const $
                    Aws.sendRequest env s3c $ getObject b ck
                Conduit.unwrapResumable $ responseBody (gorResponse gor)
            src >> lift fin >> chunkSource env s3c b cc

listChunks :: S3Resumable -> ExceptT Error App (Maybe (Seq S3Chunk))
listChunks r = do
    let ast = resumableAsset r
    let S3ResumableKey key = resumableKey r
    Log.debug $ "remote" .= val "S3"
        ~~ "asset" .= toByteString ast
        ~~ "asset.resumable" .= key
        ~~ msg (val "Listing chunks")
    b <- s3Bucket <$> view aws
    fmap Seq.fromList <$> case resumableUploadId r of
        Nothing -> listBucket b key
        Just up -> listParts b up
  where
    listBucket b k = do
        (_, gbr) <- tryS3 . recovering x3 handlers . const . exec $ GetBucket
            { gbBucket    = b
            , gbDelimiter = Nothing
            , gbMarker    = Nothing
            , gbMaxKeys   = Just (fromIntegral maxTotalChunks)
            , gbPrefix    = Just (k <> "/")
            }
        return . Just $ mapMaybe chunkFromObject (gbrContents gbr)

    listParts b up = do
        (_, lpr) <- tryS3 . recovering x3 handlers . const . exec $ ListParts
            { lpBucket = b
            , lpObject = s3Key (mkKey (resumableAsset r))
            , lpUploadId = up
            }
        return $ map chunkFromPart <$> lprsParts lpr

    chunkFromObject o = do
        nr <- parseNr (objectKey o)
        let etag = S3ETag (objectETag o)
        let size = fromIntegral (objectSize o)
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
                 <$> lookup hAmzMetaToken h

getAmzMetaUploadExpires :: [(Text, Text)] -> Maybe UTCTime
getAmzMetaUploadExpires h = readMay . C8.unpack . encodeUtf8
                          =<< lookup hAmzMetaUploadExpires h

getAmzMetaTotalSize :: [(Text, Text)] -> Maybe V3.TotalSize
getAmzMetaTotalSize = parseAmzMeta hAmzMetaSize

getAmzMetaChunkSize :: [(Text, Text)] -> Maybe V3.ChunkSize
getAmzMetaChunkSize = parseAmzMeta hAmzMetaChunkSize

getAmzMetaUploadId :: [(Text, Text)] -> Maybe Text
getAmzMetaUploadId = lookup hAmzMetaUploadId

parseAmzMeta :: FromByteString a => Text -> [(Text, Text)] -> Maybe a
parseAmzMeta k h = lookup k h >>= fromByteString . encodeUtf8

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

parseMIMEType :: ByteString -> Maybe MIME.Type
parseMIMEType = MIME.parseMIMEType . decodeLatin1

encodeMIMEType :: MIME.Type -> ByteString
encodeMIMEType = Text.encodeUtf8 . MIME.showType

octets :: MIME.Type
octets = MIME.Type (MIME.Application "octet-stream") []

--------------------------------------------------------------------------------
-- A custom HeadObject / HeadObjectResponse command pair, so we can
-- have access to some more metadata, like the Content-Type header
-- which we need to preserve when updating asset metadata in S3.
-- This should not be necessary any longer once cargohold is migrated
-- to use 'amazonka' instead of the 'aws' package.

newtype HeadObjectX = HeadObjectX HeadObject
    deriving (Show)

headObjectX :: Text -> Text -> HeadObjectX
headObjectX bucket key = HeadObjectX (headObject bucket key)

data HeadObjectResponseX = HeadObjectResponseX
    { horxContentType :: Maybe ByteString
    , horxMetadata    :: Maybe ObjectMetadata
    }

instance ResponseConsumer HeadObjectX HeadObjectResponseX where
    type ResponseMetadata HeadObjectResponseX = S3Metadata
    responseConsumer rq (HeadObjectX ho) meta rsp = do
        hor <- responseConsumer rq ho meta rsp
        let ct = lookup "Content-Type" (responseHeaders rsp)
        return $! HeadObjectResponseX ct (horMetadata hor)

instance Aws.Transaction HeadObjectX HeadObjectResponseX

instance SignQuery HeadObjectX where
    type ServiceConfiguration HeadObjectX = S3Configuration
    signQuery (HeadObjectX ho) = signQuery ho

--------------------------------------------------------------------------------
-- S3 Multipart "List Parts" Operation
-- The 'aws' package does not currently provide this operation, so we
-- have our own minimal implementation. This should no longer be necessary
-- once cargohold is migrated to use 'amazonka'.

data ListParts = ListParts
    { lpUploadId :: Text
    , lpBucket   :: Text
    , lpObject   :: Text
    }

newtype ListPartsResponse = ListPartsResponse
    { lprsParts :: Maybe [PartInfo]
    }

data PartInfo = PartInfo
    { piNr   :: Word
    , piETag :: Text
    , piSize :: Word
    }

instance SignQuery ListParts where
    type ServiceConfiguration ListParts = S3Configuration
    signQuery ListParts {..} = s3SignQuery S3Query
        { s3QMethod = Aws.Get
        , s3QBucket = Just $! Text.encodeUtf8 lpBucket
        , s3QObject = Just $! Text.encodeUtf8 lpObject
        , s3QSubresources = toQuery [ ("uploadId" :: ByteString, Just lpUploadId) ]
        , s3QQuery = []
        , s3QContentType = Nothing
        , s3QContentMd5 = Nothing
        , s3QAmzHeaders = []
        , s3QOtherHeaders = []
        , s3QRequestBody = Nothing
        }

instance ResponseConsumer ListParts ListPartsResponse where
    type ResponseMetadata ListPartsResponse = S3Metadata

    responseConsumer rq _ rm rs
        | status == status200 = parse rm rs
        | status == status404 = return $ ListPartsResponse Nothing
        | otherwise = throwStatusCodeException rq rs
      where
        status = responseStatus rs

        parse = s3XmlResponseConsumer $ \cursor -> do
            parts <- sequence $ cursor $/ laxElement "Part" &| parsePart
            return $! ListPartsResponse (Just parts)

        parsePart el = do
            nr <- Aws.force "Missing Part Number" $ el $/ Aws.elContent "PartNumber"
            et <- Aws.force "Missing ETag"        $ el $/ Aws.elContent "ETag"
            sz <- Aws.force "Missing Size"        $ el $/ Aws.elContent "Size"
            PartInfo <$> Aws.textReadInt nr <*> pure et <*> Aws.textReadInt sz

instance Aws.Transaction ListParts ListPartsResponse

instance Aws.AsMemoryResponse ListPartsResponse where
    type MemoryResponse ListPartsResponse = ListPartsResponse
    loadToMemory = return

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
          . lookup "zasset"

getOtrMetadata :: ConvId -> AssetId -> ExceptT Error App (Maybe UserId)
getOtrMetadata cnv ast = do
    b <- s3Bucket <$> view aws
    let S3AssetKey key = otrKey cnv ast
    (_, r) <- tryS3 . recovering x3 handlers . const $ exec (headObject b key)
    return $ (omUserMetadata <$> horMetadata r) >>= getAmzMetaUser

