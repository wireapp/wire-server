module Wire.ParseException where

import Data.Text qualified as Text
import Imports

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
