module CargoHold.Util where

import CargoHold.App
import CargoHold.Options
import Control.Lens
import Data.ByteString.Conversion
import URI.ByteString hiding (urlEncode)

import qualified CargoHold.CloudFront    as CloudFront
import qualified CargoHold.S3            as S3

genSignedURI :: (ToByteString p) => p -> Handler URI
genSignedURI path = do
    disableCF <- view (settings.setDisableCloudFront)
    if disableCF
        then S3.signedUrl path
        else do
            clf <- view cloudFront
            CloudFront.signedUrl clf path
