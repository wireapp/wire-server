module CargoHold.Util where

import CargoHold.App
import qualified CargoHold.CloudFront as CloudFront
import qualified CargoHold.S3 as S3
import Control.Lens
import Data.ByteString.Conversion
import Imports
import URI.ByteString hiding (urlEncode)

genSignedURL :: (ToByteString p) => p -> Handler URI
genSignedURL path = do
  uri <- cloudFront <$> view aws >>= \case
    Nothing -> S3.signedURL path
    Just cf -> CloudFront.signedURL cf path
  return $! uri
