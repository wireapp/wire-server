{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE BangPatterns      #-}

module CargoHold.Example where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LB
import           Data.Conduit
import qualified Data.Conduit.Binary     as CB
import qualified Data.Conduit.List       as CL
import qualified Data.Foldable           as Fold
import           Data.Monoid
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeLatin1, encodeUtf8)
import qualified Data.Text.IO            as Text
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.Data.Body
import           Network.AWS.S3
import           System.IO

testAmazonka :: IO ()
testAmazonka = putChunkedFileMany
    Ireland
    (BucketName "w-tmp")
    (ObjectKey "test-file.txt")
    defaultChunkSize
    (ObjectKey "test-file-hello.txt")
    (ObjectKey "test-file-world.txt")

putChunkedFileMany :: Region     -- ^ Region to operate in.
               -> BucketName -- ^ The bucket to store the file in.
               -> ObjectKey  -- ^ The destination object key.
               -> ChunkSize  -- ^ The chunk size to send.
               -> ObjectKey   -- ^ The source file to upload.
               -> ObjectKey   -- ^ The source file to upload.
               -> IO ()
putChunkedFileMany r b k c f1 f2 = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    runResourceT . runAWST env $ do
        rs1 <- send (getObject b f1)
        let bs1 = view gorsBody rs1
        void . send $ putObject b k (fn bs1)
        say $ "Successfully Uploaded Files to " <> toText b <> " - " <> toText k

    getFileContents Ireland (BucketName "w-tmp") (ObjectKey "test-file.txt")
  where
    fn :: RsBody -> RqBody
    fn (RsBody b1) = Chunked $ ChunkedBody (ChunkSize 8192) 287523 (toSource b1)

    toSource :: ResumableSource (ResourceT IO) ByteString -> Source (ResourceT IO) ByteString
    toSource r = do
        -- unwrapResumable :: MonadIO m => ResumableSource m o -> m (Source m o, m ())
        -- (src, fin) <- liftIO $ runResourceT (unwrapResumable r)
        -- return src
        v <- (RsBody r) `sinkBody` CB.sinkLbs
        cheap (LB.splitAt 8192 v)

    cheap :: (LB.ByteString, LB.ByteString) -> Source (ResourceT IO) ByteString
    cheap (x, xs) = if LB.null xs
                        then yield (LB.toStrict x) -- This assumes ALL parts have at least 8192 B
                        else yield (LB.toStrict x) >> cheap (LB.splitAt 8192 xs)

    fn' :: RsBody -> RqBody
    fn' b = Chunked $ ChunkedBody (ChunkSize 8192) 287523 (toSource' b)

    toSource' :: RsBody -> Source (ResourceT IO) ByteString
    toSource' r = do
        v <- r `sinkBody` CB.sinkLbs
        yield (LB.toStrict v)

    -- fn :: RsBody -> RsBody -> RqBody
    -- fn (RsBody b1) (RsBody b2) = Chunked $ ChunkedBody (ChunkSize 8192) 8192 (toSource b2)

    -- fn' :: RsBody -> RsBody -> ChunkedBody
    -- fn' (RsBody b1) (RsBody b2) = ChunkedBody (ChunkSize 8192) 8192 (toSource b2)

    -- toSource :: ResumableSource (ResourceT IO) ByteString -> Source (ResourceT IO) ByteString
    -- toSource r = fst =<< (liftIO $ runResourceT (unwrapResumable r))

    -- toSource' :: RsBody -> Source (ResourceT IO) ByteString
    -- toSource' (RsBody b) = toSource b

    -- fn (Chunked (ChunkedBody s1 l1 b1)) (Chunked (ChunkedBody s2 l2 b2)) =
    --     Chunked $ ChunkedBody s2 (l1 + l2) (sequence_ [b1, b2])


-- testAmazonka :: IO ()
-- testAmazonka = putChunkedFileMany
--     Ireland
--     (BucketName "w-tmp")
--     (ObjectKey "test-file.txt")
--     defaultChunkSize
--     "/tmp/test-file-hello.txt"
--     "/tmp/test-file-world.txt"

-- putChunkedFileMany :: Region     -- ^ Region to operate in.
--                -> BucketName -- ^ The bucket to store the file in.
--                -> ObjectKey  -- ^ The destination object key.
--                -> ChunkSize  -- ^ The chunk size to send.
--                -> FilePath   -- ^ The source file to upload.
--                -> FilePath   -- ^ The source file to upload.
--                -> IO ()
-- putChunkedFileMany r b k c f1 f2 = do
--     lgr <- newLogger Debug stdout
--     env <- newEnv Discover <&> set envLogger lgr . set envRegion r

--     runResourceT . runAWST env $ do
--         bdy1 <- chunkedFile c f1
--         bdy2 <- chunkedFile c f2
--         void . send $ putObject b k (fn bdy1 bdy2)
--         say $ "Successfully Uploaded Files to " <> toText b <> " - " <> toText k

--     getFileContents Ireland (BucketName "w-tmp") (ObjectKey "test-file.txt")
--   where
--     fn :: RqBody -> RqBody -> RqBody
--     fn (Chunked (ChunkedBody s1 l1 b1)) (Chunked (ChunkedBody s2 l2 b2)) =
--         Chunked $ ChunkedBody s2 (l1 + l2) (sequence_ [b1, b2])

putChunkedFile :: Region     -- ^ Region to operate in.
               -> BucketName -- ^ The bucket to store the file in.
               -> ObjectKey  -- ^ The destination object key.
               -> ChunkSize  -- ^ The chunk size to send.
               -> FilePath   -- ^ The source file to upload.
               -> IO ()
putChunkedFile r b k c f = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    runResourceT . runAWST env $ do
        bdy <- chunkedFile c f
        void . send $ putObject b k bdy
        say $ "Successfully Uploaded: "
           <> toText f <> " to " <> toText b <> " - " <> toText k

getPresignedURL :: Region     -- ^ Region to operate in.
                -> BucketName
                -> ObjectKey  -- ^ The source object key.
                -> IO ByteString
getPresignedURL r b k = do
    lgr <- newLogger Trace stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r
    ts  <- getCurrentTime
    runResourceT . runAWST env $
        presignURL ts 60 (getObject b k)

listAll :: Region -- ^ Region to operate in.
        -> IO ()
listAll r = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    let val :: ToText a => Maybe a -> Text
        val   = maybe "Nothing" toText

        lat v = maybe mempty (mappend " - " . toText) (v ^. ovIsLatest)
        key v = val (v ^. ovKey) <> ": " <> val (v ^. ovVersionId) <> lat v

    runResourceT . runAWST env $ do
        say "Listing Buckets .."
        bs <- view lbrsBuckets <$> send listBuckets
        say $ "Found " <> toText (length bs) <> " Buckets."

        forM_ bs $ \(view bName -> b) -> do
            say $ "Listing Object Versions in: " <> toText b
            paginate (listObjectVersions b)
                =$= CL.concatMap (view lovrsVersions)
                 $$ CL.mapM_     (say . mappend " -> " . key)

getFile :: Region     -- ^ Region to operate in.
        -> BucketName
        -> ObjectKey  -- ^ The source object key.
        -> FilePath   -- ^ The destination file to save as.
        -> IO ()
getFile r b k f = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    runResourceT . runAWST env $ do
        rs <- send (getObject b k)
        view gorsBody rs `sinkBody` CB.sinkFile f
        say $ "Successfully Download: "
            <> toText b <> " - " <> toText k <> " to " <> toText f

getFileContents :: Region     -- ^ Region to operate in.
        -> BucketName
        -> ObjectKey  -- ^ The source object key.
        -> IO ()
getFileContents r b k = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    runResourceT . runAWST env $ do
        rs <- send (getObject b k)
        bs <- (view gorsBody rs) `sinkBody` CB.sinkLbs
        say $ "Successfully Download: "
            <> toText b <> " - " <> toText k <> " contents: '" <> decodeLatin1 (LB.toStrict bs) <> "'"

tagBucket :: Region              -- ^ Region to operate in.
          -> BucketName          -- ^ Name of the bucket to tag.
          -> [(ObjectKey, Text)] -- ^ List of K/V pairs to apply as tags.
          -> IO ()
tagBucket r  bkt xs = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr . set envRegion r

    let tags = map (uncurry tag) xs
        kv t = toText (t ^. tagKey) <> "=" <> (t ^. tagValue)

    runResourceT . runAWST env $ do
        void . send $ putBucketTagging bkt (tagging & tTagSet .~ tags)
        say $ "Successfully Put Tags: " <> Fold.foldMap kv tags

        ts <- view gbtrsTagSet <$> send (getBucketTagging bkt)
        forM_ ts $ \t ->
            say $ "Found Tag: " <> kv t

say :: MonadIO m => Text -> m ()
say = liftIO . Text.putStrLn
