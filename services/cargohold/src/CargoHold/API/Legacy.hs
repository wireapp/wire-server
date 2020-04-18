module CargoHold.API.Legacy
  ( download,
    downloadOtr,
  )
where

import CargoHold.App
import qualified CargoHold.S3 as S3
import CargoHold.Util
import Data.Id
import Imports
import URI.ByteString

download :: UserId -> ConvId -> AssetId -> Handler (Maybe URI)
download _ _ ast = S3.getMetadata ast >>= maybe notFound found
  where
    notFound = return Nothing
    found public =
      if not public
        then return Nothing
        else do
          url <- genSignedURL (S3.plainKey ast)
          return $! Just $! url

downloadOtr :: UserId -> ConvId -> AssetId -> Handler (Maybe URI)
downloadOtr _ cnv ast = S3.getOtrMetadata cnv ast >>= maybe notFound found
  where
    notFound = return Nothing
    found _ = do
      url <- genSignedURL (S3.otrKey cnv ast)
      return $! Just $! url
