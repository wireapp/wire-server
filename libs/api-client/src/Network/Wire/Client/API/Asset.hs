{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.Asset
    ( AssetData
    , AssetKey
    , AssetSettings
    , AssetToken
    , Asset
    , assetKey
    , assetToken
    , assetExpires
    , defAssetSettings
    , setAssetPublic
    , setAssetRetention
    , postAsset
    , getAsset
    ) where

import Bilge
import CargoHold.Types
-- import Crypto.Hash (Digest, hashlazy)
-- import Crypto.Hash.Algorithms (MD5 (..))
-- import CargoHold.Types.V3
import Control.Monad.IO.Class
-- import Data.Aeson (encode, ToJSON)
-- import Data.ByteArray (convert)
-- import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Conversion
-- import Data.Id
import Data.List.NonEmpty
-- import Data.Monoid ((<>))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session
-- import Network.Wire.Client.API.Push (ConvEvent)

import qualified Codec.MIME.Type        as MIME
-- import qualified Data.ByteString.Base64 as B64
-- import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as Lazy

type AssetData = Lazy.ByteString

postAsset :: MonadSession m
          => MIME.Type
          -> AssetSettings
          -> AssetData
          -> m Asset
postAsset ctyp sets dat = sessionRequest req rsc readBody
  where
    req = method POST
        . paths ["assets", "v3"]
        . acceptJson
        . header "Content-Type" "multipart/mixed"
        . body (RequestBodyLBS $ toLazyByteString $ buildMultipartBody sets ctyp dat)
        $ empty
    rsc = status201 :| []

getAsset :: MonadSession m => AssetKey -> Maybe AssetToken -> m (Maybe AssetData)
getAsset key tok = do
    rs <- sessionRequest req rsc consumeBody
    liftIO $ case statusCode rs of
        200 -> maybe (unexpected rs "getAsset: missing body") (return . Just) (responseBody rs)
        404 -> return Nothing
        _   -> unexpected rs "getAsset: response code"
  where
    req = method GET
        . paths ["assets", "v3", toByteString' key]
        . maybe id (header "Asset-Token" . toByteString') tok
        $ empty
    rsc = status200 :| [status404]

