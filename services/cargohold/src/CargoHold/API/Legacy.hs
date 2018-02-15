{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- Legacy:
module CargoHold.API.Legacy
    ( download
    , downloadOtr
    ) where

import CargoHold.App
import CargoHold.API.Error
import Control.Error
import Control.Lens (view)
import Data.Id
import URI.ByteString

import qualified CargoHold.CloudFront as CloudFront
import qualified CargoHold.S3         as S3

download :: UserId -> ConvId -> AssetId -> Handler (Maybe URI)
download _ _ ast = S3.getMetadata ast >>= maybe notFound found
  where
    notFound = return Nothing
    found public =
        if not public
            then return Nothing
            else do
                clf <- cloudFront <$> view aws
                url <- CloudFront.signedUrl clf (S3.plainKey ast)
                case url of
                    Just u  -> return $! Just $! u
                    Nothing -> throwE serverError

downloadOtr :: UserId -> ConvId -> AssetId -> Handler (Maybe URI)
downloadOtr _ cnv ast = S3.getOtrMetadata cnv ast >>= maybe notFound found
  where
    notFound = return Nothing
    found _  = do
        clf <- cloudFront <$> view aws
        url <- CloudFront.signedUrl clf (S3.otrKey cnv ast)
        case url of
            Just u  -> return $! Just $! u
            Nothing -> throwE serverError

