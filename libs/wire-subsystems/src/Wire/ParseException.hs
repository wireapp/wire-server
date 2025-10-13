module Wire.ParseException where

import Data.Text qualified as Text
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities
import Network.Wai.Utilities.JSONResponse
import Wire.API.Error
import Wire.Error

-- | Failed to parse a response from another service.
data ParseException = ParseException
  { _parseExceptionRemote :: !Text,
    _parseExceptionMsg :: String
  }
  deriving stock (Eq, Ord, Show)

instance Exception ParseException where
  displayException (ParseException r m) =
    "Failed to parse response from remote "
      ++ Text.unpack r
      ++ " with message: "
      ++ m

instance APIError ParseException where
  toResponse _ = waiErrorToJSONResponse $ mkError status500 "internal-error" "Internal server error"

parseExceptionToHttpError :: ParseException -> HttpError
parseExceptionToHttpError (ParseException _ _) = StdError (mkError status500 "internal-error" mempty)
