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
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import Conduit
import Control.Error (ExceptT, throwE)
import Control.Lens hiding (parts, (.=), (:<), (:>))
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit.Binary as Conduit
import qualified Data.HashMap.Lazy as HML
import Data.Id
import qualified Data.Text as Text
import qualified Data.Text.Ascii as Ascii
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import qualified Data.UUID as UUID
import Imports
import Network.AWS hiding (Error)
import Network.AWS.Data.Body
import Network.AWS.S3
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
  Conduit.ConduitM () ByteString (ResourceT IO) () ->
  ExceptT Error App ()
uploadV3 prc (s3Key . mkKey -> key) originalHeaders@(V3.AssetHeaders _ cl) tok src = do
  Log.info $
    "remote" .= val "S3"
      ~~ "asset.owner" .= toByteString prc
      ~~ "asset.key" .= key
      ~~ "asset.type_from_request_ignored" .= MIME.showType (V3.hdrType originalHeaders)
      ~~ "asset.type" .= MIME.showType ct
      ~~ "asset.size" .= cl
      ~~ msg (val "Uploading asset")
  void $ exec req
  where
    ct :: MIME.Type
    ct = octets -- See note on overrideMimeTypeAsOctetStream
    stream :: ConduitM () ByteString (ResourceT IO) ()
    stream =
      src
        -- Rechunk bytestream to ensure we satisfy AWS's minimum chunk size
        -- on uploads
        .| Conduit.chunksOfCE (fromIntegral defaultChunkSize)
        -- Ignore any 'junk' after the content; take only 'cl' bytes.
        .| Conduit.isolate (fromIntegral cl)

    reqBdy :: ChunkedBody
    reqBdy = ChunkedBody defaultChunkSize (fromIntegral cl) stream

    req :: Text -> PutObject
    req b =
      putObject (BucketName b) (ObjectKey key) (toBody reqBdy)
        & poContentType ?~ MIME.showType ct
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

parseAmzMeta :: FromByteString a => Text -> [(Text, Text)] -> Maybe a
parseAmzMeta k h = lookupCI k h >>= fromByteString . encodeUtf8

-------------------------------------------------------------------------------
-- Utilities

octets :: MIME.Type
octets = MIME.Type (MIME.Application "octet-stream") []

exec :: (AWSRequest r, Show r) => (Text -> r) -> ExceptT Error App (Rs r)
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
