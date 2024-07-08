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

module CargoHold.API.V3
  ( upload,
    download,
    downloadUnsafe,
    checkMetadata,
    delete,
    renewToken,
    deleteToken,
    randToken,
  )
where

import CargoHold.API.Error
import CargoHold.API.Util
import CargoHold.App
import qualified CargoHold.Metrics as Metrics
import CargoHold.Options
import qualified CargoHold.S3 as S3
import CargoHold.Types.V3
import qualified CargoHold.Types.V3 as V3
import CargoHold.Util
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Conduit
import Control.Applicative (optional)
import Control.Error
import Control.Lens (set, view, (^.))
import Control.Monad.Trans.Resource
import Crypto.Random (getRandomBytes)
import Data.Aeson (eitherDecodeStrict')
import Data.Attoparsec.ByteString.Char8
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.Conduit.Attoparsec as Conduit
import Data.Id
import qualified Data.List as List
import Data.Qualified
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1)
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Data.UUID.V4
import Imports hiding (take)
import Network.HTTP.Types.Header
import Network.Wai.Utilities (Error (..))
import URI.ByteString
import Wire.API.Asset

upload :: V3.Principal -> ConduitM () ByteString (ResourceT IO) () -> Handler (Asset' (Local AssetKey))
upload own bdy = do
  (rsrc, sets) <- parseMetadata bdy assetSettings
  (src, hdrs) <- parseHeaders rsrc assetHeaders
  let cl = fromIntegral $ hdrLength hdrs
  when (cl <= 0) $
    throwE invalidLength
  maxBytes <- view (CargoHold.App.settings . maxTotalBytes)
  when (cl > maxBytes) $
    throwE assetTooLarge
  ast <- liftIO $ Id <$> nextRandom
  tok <- if sets ^. V3.setAssetPublic then pure Nothing else Just <$> randToken
  let ret = fromMaybe V3.AssetPersistent (sets ^. V3.setAssetRetention)
  key <- qualifyLocal (V3.AssetKeyV3 ast ret)
  void $ S3.uploadV3 own (tUnqualified key) hdrs tok src
  Metrics.s3UploadOk
  Metrics.s3UploadSize cl
  expires <- case V3.assetRetentionSeconds ret of
    Just n -> Just . addUTCTime n <$> liftIO getCurrentTime
    Nothing -> pure Nothing
  pure $!
    V3.mkAsset key
      & set V3.assetExpires expires
      & set V3.assetToken tok

renewToken :: V3.Principal -> V3.AssetKey -> Handler V3.AssetToken
renewToken own key = do
  tok <- randToken
  updateToken own key (Just tok)
  pure tok

deleteToken :: V3.Principal -> V3.AssetKey -> Handler ()
deleteToken own key = updateToken own key Nothing

updateToken :: V3.Principal -> V3.AssetKey -> Maybe V3.AssetToken -> Handler ()
updateToken own key tok = do
  m <- S3.getMetadataV3 key >>= maybe (throwE assetNotFound) pure
  unless (S3.v3AssetOwner m == own) $
    throwE unauthorised
  let m' = m {S3.v3AssetToken = tok}
  S3.updateMetadataV3 key m'

randToken :: (MonadIO m) => m V3.AssetToken
randToken = liftIO $ V3.AssetToken . Ascii.encodeBase64Url <$> getRandomBytes 16

download :: V3.Principal -> V3.AssetKey -> Maybe V3.AssetToken -> Maybe Text -> Handler (Maybe URI)
download own key tok mbHost = runMaybeT $ do
  checkMetadata (Just own) key tok
  lift $ genSignedURL (S3.mkKey key) mbHost

downloadUnsafe :: V3.AssetKey -> Maybe Text -> Handler URI
downloadUnsafe key mbHost = genSignedURL (S3.mkKey key) mbHost

checkMetadata :: Maybe V3.Principal -> V3.AssetKey -> Maybe V3.AssetToken -> MaybeT Handler ()
checkMetadata mown key tok = do
  s3 <- lift (S3.getMetadataV3 key) >>= maybe mzero pure
  guard $ mown == Just (S3.v3AssetOwner s3) || tok == S3.v3AssetToken s3

delete :: V3.Principal -> V3.AssetKey -> Handler ()
delete own key = do
  m <- S3.getMetadataV3 key >>= maybe (throwE assetNotFound) pure
  unless (S3.v3AssetOwner m == own) $
    throwE unauthorised
  S3.deleteV3 key

-----------------------------------------------------------------------------
-- Streaming multipart parsing

parseMetadata :: ConduitM () ByteString (ResourceT IO) () -> Parser a -> Handler (SealedConduitT () ByteString (ResourceT IO) (), a)
parseMetadata src psr = do
  (rsrc, meta) <- liftIO . runResourceT $ src $$+ sinkParser psr
  (rsrc,) <$> hoistEither meta

parseHeaders :: SealedConduitT () ByteString (ResourceT IO) () -> Parser a -> Handler (ConduitM () ByteString (ResourceT IO) (), a)
parseHeaders src prs = do
  (rsrc, hdrs) <- liftIO $ runResourceT $ src $$++ sinkParser prs
  let src' = Conduit.unsealConduitT rsrc
  (src',) <$> hoistEither hdrs

sinkParser :: Parser a -> ConduitM ByteString o (ResourceT IO) (Either Error a)
sinkParser p = fmapL mkError <$> Conduit.sinkParserEither p
  where
    mkError = clientError . LT.pack . mkMsg
    mkMsg e =
      "Expected: "
        ++ intercalate ", " (Conduit.errorContexts e)
        ++ ", "
        ++ Conduit.errorMessage e
        ++ " at "
        ++ show (Conduit.errorPosition e)

-- Parsing Primitives

assetSettings :: Parser V3.AssetSettings
assetSettings = do
  (ct, cl) <- metadataHeaders
  unless (MIME.mimeType ct == MIME.Application "json") $
    fail "Invalid metadata Content-Type. Expected 'application/json'."
  bs <- take (fromIntegral cl)
  either fail pure (eitherDecodeStrict' bs)

metadataHeaders :: Parser (MIME.Type, Word)
metadataHeaders =
  optional eol
    *> boundary
    *> (headers [hContentType, hContentLength] >>= go)
    <* eol
  where
    go hdrs = do
      ct <- contentType hdrs
      cl <- contentLength hdrs
      pure (ct, cl)

assetHeaders :: Parser CargoHold.Types.V3.AssetHeaders
assetHeaders =
  eol
    *> boundary
    *> (headers [hContentType, hContentLength, hContentMD5] >>= go)
    <* eol
  where
    go hdrs =
      CargoHold.Types.V3.AssetHeaders
        <$> contentType hdrs
        <*> contentLength hdrs

contentType :: [(HeaderName, ByteString)] -> Parser MIME.Type
contentType hdrs =
  maybe
    (fail "Missing Content-Type")
    (maybe (fail "Invalid MIME type") pure . MIME.parseMIMEType . decodeLatin1)
    (lookup (CI.mk "Content-Type") hdrs)

contentLength :: [(HeaderName, ByteString)] -> Parser Word
contentLength hdrs =
  maybe
    (fail "Missing Content-Type")
    (either fail pure . parseOnly decimal)
    (lookup (CI.mk "Content-Length") hdrs)

boundary :: Parser ()
boundary =
  char '-'
    *> char '-'
    *> takeTill isEOL
    *> eol
    <?> "MIME boundary"

-- | Always parses until the end of headers is reached
-- (a line not starting with a char that's valid in header names, usually an empty line),
-- or fails.
-- Not all listed headers must be found, but other headers (or duplicates) raise an error.
headers :: [HeaderName] -> Parser [(HeaderName, ByteString)]
headers allowed = do
  -- optional in case there is no header left to parse
  optional (CI.mk <$> takeWhile1 (\c -> isAlphaNum c || c == '-') <?> "header name") >>= \case
    Nothing ->
      pure []
    Just name
      | name `notElem` allowed ->
          -- might also be a duplicate
          fail $ "Unexpected header: " ++ show (CI.original name)
      | otherwise -> do
          _ <- char ':'
          skipSpace
          value <- takeTill isEOL <?> "header value"
          eol
          -- we don't want to parse it again (this also ensures quick termination)
          ((name, value) :) <$> headers (List.delete name allowed)

eol :: Parser ()
eol = endOfLine <?> "\r\n"

isEOL :: Char -> Bool
isEOL c = c == '\n' || c == '\r'
