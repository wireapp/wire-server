module Wire.Error where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities.Error qualified as Wai

-- | Error thrown to the user
data HttpError where
  StdError :: !Wai.Error -> HttpError
  RichError :: (ToJSON a) => !Wai.Error -> !a -> [Header] -> HttpError

instance Show HttpError where
  show (StdError werr) = "StdError (" <> show werr <> ")"
  show e@(RichError _ _ headers) = "RichError (json = " <> Text.unpack (Text.decodeUtf8 $ BS.toStrict $ encode e) <> ", headers = " <> show headers <> ")"

instance Exception HttpError

errorLabel :: HttpError -> LText
errorLabel (StdError e) = Wai.label e
errorLabel (RichError e _ _) = Wai.label e

instance ToJSON HttpError where
  toJSON (StdError e) = toJSON e
  toJSON (RichError e x _) = case (toJSON e, toJSON x) of
    (Object o1, Object o2) -> Object (KeyMap.union o1 o2)
    (j, _) -> j
