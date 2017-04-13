{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module CargoHold.API.V3
    ( upload
    , download
    , delete
    , renewToken
    , deleteToken
    , randToken
    ) where

import CargoHold.App
import CargoHold.API.Error
import CargoHold.Types.V3
import Control.Applicative
import Control.Error
import Control.Lens (view, (^.), set, (&))
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash
import Data.Aeson (eitherDecodeStrict')
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Conduit (Source, ResumableSource, Consumer, ($=), ($$+), ($$++))
import Data.Id
import Data.List (intercalate)
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock
import Data.UUID.V4
import Network.HTTP.Types.Header
import Network.Wai.Utilities (Error (..))
import OpenSSL.Random (randBytes)
import Prelude hiding (take)
import URI.ByteString

import qualified CargoHold.CloudFront    as CloudFront
import qualified CargoHold.Metrics       as Metrics
import qualified CargoHold.S3            as S3
import qualified CargoHold.Types.V3      as V3
import qualified Codec.MIME.Type         as MIME
import qualified Codec.MIME.Parse        as MIME
import qualified Data.ByteString.Base64  as B64
import qualified Data.CaseInsensitive    as CI
import qualified Data.Conduit            as Conduit
import qualified Data.Conduit.Attoparsec as Conduit
import qualified Data.Conduit.Binary     as Conduit
import qualified Data.Text.Ascii         as Ascii
import qualified Data.Text.Lazy          as LT

upload :: V3.Principal -> Source IO ByteString -> Handler V3.Asset
upload own bdy = do
    (rsrc, sets) <- parseMetadata bdy assetSettings
    (src,  hdrs) <- parseHeaders rsrc assetHeaders
    let cl = fromIntegral $ hdrLength hdrs
    when (cl <= 0) $
        throwE invalidLength
    maxTotalBytes <- view maxTotalUpload
    when (cl > maxTotalBytes) $
        throwE assetTooLarge
    let stream = src $= Conduit.isolate cl
    ast <- liftIO $ Id <$> nextRandom
    tok <- if sets^.V3.setAssetPublic then return Nothing else Just <$> randToken
    let ret = fromMaybe V3.AssetPersistent (sets^.V3.setAssetRetention)
    let key = V3.AssetKeyV3 ast ret
    void $ S3.uploadV3 own key hdrs tok stream
    Metrics.s3UploadOk
    Metrics.s3UploadSize cl
    expires <- case V3.assetRetentionSeconds ret of
        Just  n -> Just . addUTCTime n <$> liftIO getCurrentTime
        Nothing -> return Nothing
    return $! V3.mkAsset key
            & set V3.assetExpires expires
            & set V3.assetToken tok

renewToken :: V3.Principal -> V3.AssetKey -> Handler V3.AssetToken
renewToken own key = do
    tok <- randToken
    updateToken own key (Just tok)
    return tok

deleteToken :: V3.Principal -> V3.AssetKey -> Handler ()
deleteToken own key = updateToken own key Nothing

updateToken :: V3.Principal -> V3.AssetKey -> Maybe V3.AssetToken -> Handler ()
updateToken own key tok = do
    m <- S3.getMetadataV3 key >>= maybe (throwE assetNotFound) return
    unless (S3.v3AssetOwner m == own) $
        throwE unauthorised
    let m' = m { S3.v3AssetToken = tok }
    S3.updateMetadataV3 key m'

randToken :: MonadIO m => m V3.AssetToken
randToken = liftIO $ V3.AssetToken . Ascii.encodeBase64Url <$> randBytes 16

download :: V3.Principal -> V3.AssetKey -> Maybe V3.AssetToken -> Handler (Maybe URI)
download own key tok = S3.getMetadataV3 key >>= maybe notFound found
  where
    notFound = return Nothing
    found s3
        | own /= S3.v3AssetOwner s3 && tok /= S3.v3AssetToken s3 = return Nothing
        | otherwise = do
            clf <- cloudFront <$> view aws
            url <- CloudFront.signedUrl clf (S3.mkKey key)
            return $! Just $! url

delete :: V3.Principal -> V3.AssetKey -> Handler ()
delete own key = do
    m <- S3.getMetadataV3 key >>= maybe (throwE assetNotFound) return
    unless (S3.v3AssetOwner m == own) $
        throwE unauthorised
    S3.deleteV3 key

-----------------------------------------------------------------------------
-- Streaming multipart parsing

parseMetadata :: Source IO ByteString -> Parser a -> Handler (ResumableSource IO ByteString, a)
parseMetadata src psr = do
    (rsrc, meta) <- liftIO $ src $$+ sinkParser psr
    (rsrc,) <$> hoistEither meta

parseHeaders :: ResumableSource IO ByteString -> Parser a -> Handler (Source IO ByteString, a)
parseHeaders src prs = do
    (rsrc, hdrs) <- liftIO $ src $$++ sinkParser prs
    (src',    _) <- liftIO $ Conduit.unwrapResumable rsrc
    (src',) <$> hoistEither hdrs

sinkParser :: Parser a -> Consumer ByteString IO (Either Error a)
sinkParser p = fmapL mkError <$> Conduit.sinkParserEither p
  where
    mkError = clientError . LT.pack . mkMsg
    mkMsg e = "Expected: " ++ intercalate ", " (Conduit.errorContexts e)
           ++ ", " ++ Conduit.errorMessage e
           ++ " at " ++ show (Conduit.errorPosition e)

-- Parsing Primitives

assetSettings :: Parser V3.AssetSettings
assetSettings = do
    (ct, cl) <- metadataHeaders
    unless (MIME.mimeType ct == MIME.Application "json") $
        fail "Invalid metadata Content-Type. Expected 'application/json'."
    bs <- take (fromIntegral cl)
    either fail return (eitherDecodeStrict' bs)

metadataHeaders :: Parser (MIME.Type, Word)
metadataHeaders = optional eol
    *> boundary
    *> (headers [hContentType, hContentLength] >>= go)
    <* eol
  where
    go hdrs = do
        ct <- contentType   hdrs
        cl <- contentLength hdrs
        return (ct, cl)

assetHeaders :: Parser AssetHeaders
assetHeaders = eol
    *> boundary
    *> (headers [hContentType, hContentLength, hContentMD5] >>= go)
    <* eol
  where
    go hdrs = AssetHeaders <$> contentType hdrs
                           <*> contentLength hdrs
                           <*> contentMD5 hdrs

contentType :: [(HeaderName, ByteString)] -> Parser MIME.Type
contentType hdrs =
    maybe (fail "Missing Content-Type")
          (maybe (fail "Invalid MIME type") return . MIME.parseMIMEType . decodeLatin1)
          (lookup (CI.mk "Content-Type") hdrs)

contentLength :: [(HeaderName, ByteString)] -> Parser Word
contentLength hdrs =
   maybe (fail "Missing Content-Type")
         (either fail return . parseOnly decimal)
         (lookup (CI.mk "Content-Length") hdrs)

contentMD5 :: [(HeaderName, ByteString)] -> Parser (Digest MD5)
contentMD5 hdrs =
    maybe (fail "Missing Content-MD5")
          (maybe (fail "Invalid Content-MD5") return . digestFromByteString . B64.decodeLenient)
          (lookup (CI.mk "Content-MD5") hdrs)

boundary :: Parser ()
boundary = char '-'
        *> char '-'
        *> takeTill isEOL
        *> eol
        <?> "MIME boundary"

headers :: [HeaderName] -> Parser [(HeaderName, ByteString)]
headers names = count (length names) (header names)

header :: [HeaderName] -> Parser (HeaderName, ByteString)
header names = do
    name <- CI.mk <$> takeTill (== ':') <?> "header name"
    unless (name `elem` names) $
        fail $ "Unexpected header: " ++ show (CI.original name)
    _ <- char ':'
    skipSpace
    value <- takeTill isEOL <?> "header value"
    eol
    return (name, value)

eol :: Parser ()
eol = endOfLine <?> "\r\n"

isEOL :: Char -> Bool
isEOL c = c == '\n' || c == '\r'
