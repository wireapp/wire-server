{-# LANGUAGE OverloadedStrings #-}

module API.V3 where

import Bilge hiding (body)
import Bilge.Assert
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Lens (view, set, (&), (^.))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.Id
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V4
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200, status204)
import Network.Wai.Utilities (Error (label))
import Prelude hiding (head)
import Test.Tasty
import Test.Tasty.HUnit

import qualified CargoHold.Types.V3           as V3
import qualified CargoHold.Types.V3.Resumable as V3
import qualified Codec.MIME.Parse             as MIME
import qualified Codec.MIME.Type              as MIME
import qualified Data.ByteString.Lazy         as Lazy
import qualified Data.ByteString.Char8        as C8
import qualified Data.UUID                    as UUID

type CargoHold = Request -> Request

data TestSetup = TestSetup
  { manager   :: Manager
  , cargohold :: CargoHold
  }

type TestSignature a = CargoHold -> Http a

test :: IO TestSetup -> TestName -> (TestSignature a) -> TestTree
test s n h = testCase n runTest
  where
    runTest = do
        setup <- s
        (void $ runHttpT (manager setup) (h (cargohold setup)))

tests :: IO TestSetup -> TestTree
tests s = testGroup "v3"
    [ testGroup "simple"
        [ test s "roundtrip"          testSimpleRoundtrip
        , test s "tokens"             testSimpleTokens
        , test s "s3-upstream-closed" testSimpleS3ClosedConnectionReuse
        ]
    , testGroup "resumable"
        [ test s "small"          testResumableSmall
        , test s "large"          testResumableBig
        , test s "last-small"     testResumableLastSmall
        , test s "stepwise-small" testResumableStepSmall
        , test s "stepwise-big"   testResumableStepBig
        ]
    ]

--------------------------------------------------------------------------------
-- Simple (single-step) uploads

testSimpleRoundtrip :: TestSignature ()
testSimpleRoundtrip c = do
    uid <- liftIO $ Id <$> nextRandom
    uid2 <- liftIO $ Id <$> nextRandom

    -- Initial upload
    let sets = V3.defAssetSettings
    let bdy = (applicationText, "Hello World")
    r1 <- uploadSimple (c . path "/assets/v3") uid sets bdy <!!
        const 201 === statusCode

    let      loc = decodeHeader "Location" r1 :: ByteString
    let Just ast = decodeBody r1 :: Maybe V3.Asset
    let Just tok = view V3.assetToken ast

    -- Check mandatory Date header and expiration.
    let Just date = C8.unpack <$> lookup "Date" (responseHeaders r1)
    let       utc = parseTimeOrError False defaultTimeLocale rfc822DateFormat date :: UTCTime
    liftIO $ assertBool "invalid expiration" (Just utc < view V3.assetExpires ast)

    -- Lookup with token and download via redirect.
    r2 <- get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok) . noRedirect) <!! do
        const 302 === statusCode
        const Nothing === responseBody
    r3 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r2))
    liftIO $ do
        assertEqual "status" status200 (responseStatus r3)
        assertEqual "content-type mismatch" (Just applicationText) (getContentType r3)
        assertEqual "token mismatch" tok (decodeHeader "x-amz-meta-token" r3)
        assertEqual "user mismatch" uid (decodeHeader "x-amz-meta-user" r3)
        assertEqual "data mismatch" (Just "Hello World") (responseBody r3)

    -- Delete (forbidden for other users)
    deleteAsset c uid2 (view V3.assetKey ast) !!! const 403 === statusCode
    -- Delete (allowed for creator)
    deleteAsset c uid (view V3.assetKey ast) !!! const 200 === statusCode
    r4 <- get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok) . noRedirect) <!!
        const 404 === statusCode
    let Just date' = C8.unpack <$> lookup "Date" (responseHeaders r4)
    let utc' = parseTimeOrError False defaultTimeLocale rfc822DateFormat date' :: UTCTime
    liftIO $ assertBool "bad date" (utc' >= utc)

testSimpleTokens :: TestSignature ()
testSimpleTokens c = do
    uid <- liftIO $ Id <$> nextRandom
    uid2 <- liftIO $ Id <$> nextRandom

    -- Initial upload
    let sets = V3.defAssetSettings & set V3.setAssetRetention (Just V3.AssetVolatile)
    let bdy = (applicationText, "Hello World")
    r1 <- uploadSimple (c . path "/assets/v3") uid sets bdy <!!
        const 201 === statusCode

    let      loc = decodeHeader "Location" r1 :: ByteString
    let Just ast = decodeBody r1 :: Maybe V3.Asset
    let      key = view V3.assetKey ast
    let Just tok = view V3.assetToken ast

    -- No access without token from other user (opaque 404)
    get (c . path loc . zUser uid2 . noRedirect) !!!
        const 404 === statusCode

    -- No access with wrong token (opaque 404)
    get (c . path loc . zUser uid2 . header "Asset-Token" "acb123" . noRedirect) !!!
        const 404 === statusCode

    -- Token renewal fails if not done by owner
    post (c . paths ["assets", "v3", toByteString' key, "token"] . zUser uid2) !!! do
        const 403 === statusCode
        const (Just "unauthorised") === fmap label . decodeBody

    -- Token renewal succeeds if done by owner
    r2 <- post (c . paths ["assets", "v3", toByteString' key, "token"] . zUser uid) <!!
        const 200 === statusCode
    let Just tok' = V3.newAssetToken <$> decodeBody r2
    liftIO $ assertBool "token unchanged" (tok /= tok')

    -- Download by owner with new token.
    r3 <- get (c . path loc . zUser uid . header "Asset-Token" (toByteString' tok') . noRedirect) <!! do
        const 302 === statusCode
        const Nothing === responseBody
    r4 <- flip get' id =<< parseUrlThrow (C8.unpack (getHeader' "Location" r3))
    liftIO $ do
        assertEqual "status" status200 (responseStatus r4)
        assertEqual "content-type mismatch" (Just applicationText) (getContentType r4)
        assertEqual "token mismatch" tok' (decodeHeader "x-amz-meta-token" r4)
        assertEqual "user mismatch" uid (decodeHeader "x-amz-meta-user" r4)
        assertEqual "data mismatch" (Just "Hello World") (responseBody r4)

    -- Verify access without token if the request comes from the creator.
    get (c . path loc . zUser uid . noRedirect) !!!
        const 302 === statusCode

    -- Verify access with new token from a different user.
    get (c . path loc . header "Asset-Token" (toByteString' tok') . zUser uid2 . noRedirect) !!!
        const 302 === statusCode

    -- Delete Token fails if not done by owner
    delete (c . paths ["assets", "v3", toByteString' key, "token"] . zUser uid2) !!! do
        const 403 === statusCode
        const (Just "unauthorised") === fmap label . decodeBody

    -- Delete Token succeeds by owner
    delete (c . paths ["assets", "v3", toByteString' key, "token"] . zUser uid) !!! do
        const 200 === statusCode
        const Nothing === responseBody

    -- Access without token from different user (asset is now "public")
    get (c . path loc . noRedirect . zUser uid2) !!! do
        const 302 === statusCode
        const Nothing === responseBody

-- S3 closes idle connections after ~5 seconds, before the http-client 'Manager'
-- does. If such a closed connection is reused for an upload, no problems should
-- occur (i.e. the closed connection should be detected before sending any data).
testSimpleS3ClosedConnectionReuse :: TestSignature ()
testSimpleS3ClosedConnectionReuse c = go >> wait >> go
  where
    wait = liftIO $ putStrLn "Waiting for S3 idle timeout ..." >> threadDelay 7000000
    go = do
        uid <- liftIO $ Id <$> nextRandom
        let sets  = V3.defAssetSettings & set V3.setAssetRetention (Just V3.AssetVolatile)
        let part2 = (MIME.Type (MIME.Text "plain") [], C8.replicate 100000 'c')
        uploadSimple (c . path "/assets/v3") uid sets part2 !!!
            const 201 === statusCode

--------------------------------------------------------------------------------
-- Resumable (multi-step) uploads

testResumableSmall :: TestSignature ()
testResumableSmall c = assertRandomResumable c totalSize chunkSize UploadFull
  where
    totalSize = 100        -- 100 B
    chunkSize = 100 * 1024 -- 100 KiB

testResumableBig :: TestSignature ()
testResumableBig c = assertRandomResumable c totalSize chunkSize UploadFull
  where
    totalSize = 25 * 1024 * 1024 -- 25 MiB
    chunkSize =  1 * 1024 * 1024 --  1 MiB

testResumableLastSmall :: TestSignature ()
testResumableLastSmall c = assertRandomResumable c totalSize chunkSize UploadFull
  where
    totalSize = 250 * 1024 + 12345 -- 250 KiB + 12345 B
    chunkSize = 100 * 1024         -- 100 KiB

testResumableStepSmall :: TestSignature ()
testResumableStepSmall c = assertRandomResumable c totalSize chunkSize UploadStepwise
  where
    totalSize = 500 * 1024 + 12345 -- 500 KiB + 12345 B
    chunkSize = 100 * 1024         -- 100 KiB

-- This should use the S3 multipart upload behind the scenes.
testResumableStepBig :: TestSignature ()
testResumableStepBig c = assertRandomResumable c totalSize chunkSize UploadStepwise
  where
    totalSize = 26 * 1024 * 1024 -- 26 MiB
    chunkSize =  5 * 1024 * 1024 -- 5 MiB

-- Assertions -----------------------------------------------------------------

data UploadType = UploadFull | UploadStepwise

assertRandomResumable :: CargoHold -> V3.TotalSize -> V3.ChunkSize -> UploadType -> Http ()
assertRandomResumable c totalSize chunkSize typ = do
    (uid, dat, ast) <- randomResumable c totalSize
    let key = ast^.V3.resumableAsset.V3.assetKey
    liftIO $ assertEqual "chunksize" chunkSize (ast^.V3.resumableChunkSize)
    case typ of
        UploadStepwise -> uploadStepwise c uid key chunkSize dat
        UploadFull     -> void $ uploadResumable c uid key 0 dat
    r <- downloadAsset c uid key Nothing
    liftIO $ do
        assertEqual "status" status200 (responseStatus r)
        assertEqual "content-type mismatch" (Just textPlain) (getContentType r)
        assertEqual "user mismatch" uid (decodeHeader "x-amz-meta-user" r)
        assertEqual "data mismatch" (Just $ Lazy.fromStrict dat) (responseBody r)

randomResumable :: CargoHold -> V3.TotalSize -> Http (UserId, ByteString, V3.ResumableAsset)
randomResumable c size = do
    uid <- liftIO $ Id <$> nextRandom
    let sets = V3.mkResumableSettings V3.AssetPersistent True textPlain
    let dat  = C8.replicate (fromIntegral size) 'a'
    ast <- createResumable c uid sets size
    return (uid, dat, ast)

-- API Calls ------------------------------------------------------------------

uploadSimple
    :: CargoHold
    -> UserId
    -> V3.AssetSettings
    -> (MIME.Type, ByteString)
    -> Http (Response (Maybe Lazy.ByteString))
uploadSimple c usr sets (ct, bs) =
    let mp = V3.buildMultipartBody sets ct (Lazy.fromStrict bs)
    in post $ c
         . method POST
         . zUser usr
         . zConn "conn"
         . content "multipart/mixed"
         . lbytes (toLazyByteString mp)

createResumable
    :: CargoHold
    -> UserId
    -> V3.ResumableSettings
    -> V3.TotalSize
    -> Http V3.ResumableAsset
createResumable c u sets size = do
    rsp <- post ( c
                . path "/assets/v3/resumable"
                . zUser u
                . header "Content-Type" "application/json"
                . header "Upload-Length" (toByteString' size)
                . lbytes (encode sets)
                ) <!! const 201 === statusCode
    let Just ast = decodeBody rsp :: Maybe V3.ResumableAsset
    let Just loc = getHeader "Location" rsp
    let loc' = "/assets/v3/resumable/" <> toByteString' (ast^.V3.resumableAsset.V3.assetKey)
    liftIO $ assertEqual "Location" loc' loc
    return ast

getResumableStatus :: CargoHold -> UserId -> V3.AssetKey -> Http V3.Offset
getResumableStatus c u k = do
    r <- head ( c
              . paths ["assets", "v3", "resumable", toByteString' k]
              . zUser u
              ) <!! const 200 === statusCode
    return $ getOffset r

uploadResumable :: CargoHold -> UserId -> V3.AssetKey -> V3.Offset -> ByteString -> Http V3.Offset
uploadResumable c u k off bs = do
    r <- patch ( c
               . paths ["assets", "v3", "resumable", toByteString' k]
               . header "Upload-Offset" (toByteString' off)
               . header "Content-Type" applicationOffset
               . zUser u
               . bytes bs
               )
    liftIO $ assertEqual "status" status204 (responseStatus r)
    return $ getOffset r

uploadStepwise :: CargoHold -> UserId -> V3.AssetKey -> V3.ChunkSize -> ByteString -> Http ()
uploadStepwise c u k s d = next 0 d
  where
    totalSize = fromIntegral (C8.length d)
    chunkSize = fromIntegral s
    next pos dat = do 
        off <- uploadResumable c u k pos (C8.take chunkSize dat)
        unless (V3.offsetBytes off == totalSize) $ do
            off' <- getResumableStatus c u k
            liftIO $ assertEqual "offset" off off'
            next off (C8.drop (fromIntegral (off - pos)) dat)

getAsset :: CargoHold -> UserId -> V3.AssetKey -> Maybe V3.AssetToken -> Http (Response (Maybe Lazy.ByteString))
getAsset c u k t = get $ c
    . paths ["assets", "v3", toByteString' k]
    . zUser u
    . maybe id (header "Asset-Token" . toByteString') t
    . noRedirect

downloadAsset :: CargoHold -> UserId -> V3.AssetKey -> Maybe V3.AssetToken -> Http (Response (Maybe Lazy.ByteString))
downloadAsset c u k t = do
    r <- getAsset c u k t <!! do
        const 302 === statusCode
        const Nothing === responseBody
    l <- parseUrlThrow (C8.unpack (getHeader' "Location" r))
    get' l id

deleteAsset :: CargoHold -> UserId -> V3.AssetKey -> Http (Response (Maybe Lazy.ByteString))
deleteAsset c u k = delete $ c . zUser u . paths ["assets", "v3", toByteString' k]

-- Utilities ------------------------------------------------------------------

type ContentType = ByteString

decodeHeader :: FromByteString a => HeaderName -> Response b -> a
decodeHeader h = fromMaybe (error $ "decodeHeader: missing or invalid header: " ++ show h)
               . fromByteString
               . getHeader' h

getOffset :: Response b -> V3.Offset
getOffset = decodeHeader "Upload-Offset"

getContentType :: Response a -> Maybe MIME.Type
getContentType = MIME.parseContentType . decodeLatin1 . getHeader' "Content-Type"

applicationText :: MIME.Type
applicationText = MIME.Type (MIME.Application "text") []

textPlain :: MIME.Type
textPlain = MIME.Type (MIME.Text "plain") []

applicationOffset :: ContentType
applicationOffset = "application/offset+octet-stream"

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . UUID.toASCIIBytes . toUUID

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

decodeBody :: FromJSON a => Response (Maybe Lazy.ByteString) -> Maybe a
decodeBody = responseBody >=> decode

