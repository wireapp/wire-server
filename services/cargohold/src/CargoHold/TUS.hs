module CargoHold.TUS
  ( createdResponse,
    headResponse,
    patchResponse,
    optionsResponse,
  )
where

import CargoHold.Types.V3.Resumable (Offset, TotalSize)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Imports
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Utilities hiding (message)

createdResponse :: ByteString -> UTCTime -> Response -> Response
createdResponse loc expiry =
  setStatus status201
    . addHeader "Location" loc
    . uploadExpires expiry

-- cf. http://tus.io/protocols/resumable-upload.html#head
headResponse :: (Offset, TotalSize) -> Response -> Response
headResponse (offset, total) =
  setStatus status200
    . addHeader "Cache-Control" "no-store"
    . hResumable
    . hOffset offset
    . hLength total

-- cf. http://tus.io/protocols/resumable-upload.html#patch
patchResponse :: Offset -> UTCTime -> Response -> Response
patchResponse offset expiry =
  setStatus status204
    . hOffset offset
    . hResumable
    . uploadExpires expiry

-- cf. http://tus.io/protocols/resumable-upload.html#options
optionsResponse :: Word -> Response -> Response
optionsResponse maxSize =
  setStatus status204
    . addHeader "Tus-Extension" "creation,expiration"
    . addHeader "Tus-Max-Size" (toByteString' maxSize)
    . hVersion
    . hResumable

-- Internal --------------------------------------------------------------------

-- cf. http://tus.io/protocols/resumable-upload.html#expiration
uploadExpires :: UTCTime -> Response -> Response
uploadExpires = addHeader "Upload-Expires" . C8.pack . time
  where
    -- Must be according to RFC 7231
    time = formatTime defaultTimeLocale "%a, %d %B %Y %H:%M:%S %Z" . utcToZonedTime gmt
    gmt = TimeZone 0 False "GMT"

hVersion :: Response -> Response
hVersion = addHeader "Tus-Version" "1.0.0"

hResumable :: Response -> Response
hResumable = addHeader "Tus-Resumable" "1.0.0"

hOffset :: Offset -> Response -> Response
hOffset = addHeader "Upload-Offset" . toByteString'

hLength :: TotalSize -> Response -> Response
hLength = addHeader "Upload-Length" . toByteString'
