{-# LANGUAGE LambdaCase #-}

module CargoHold.Util where

import Imports
import CargoHold.App
import CargoHold.AWS
import Control.Lens
import Data.ByteString.Conversion
import URI.ByteString hiding (urlEncode)

import qualified CargoHold.CloudFront as CloudFront
import qualified CargoHold.S3         as S3

genSignedURL :: (ToByteString p) => p -> Handler URI
genSignedURL path = do
    uri <- view (aws.cloudFront) >>= \case
        Nothing -> S3.signedURL path
        Just cf -> CloudFront.signedURL cf path
    return $! uri
