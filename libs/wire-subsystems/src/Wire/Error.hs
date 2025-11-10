{-# LANGUAGE RecordWildCards #-}

module Wire.Error where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Lazy.Encoding qualified as LText
import Hasql.Pool
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.JSONResponse
import Servant (ServerError (..))

-- | Error thrown to the user
data HttpError where
  StdError :: !Wai.Error -> HttpError
  RichError :: (ToJSON a) => !Wai.Error -> !a -> [Header] -> HttpError

instance Eq HttpError where
  StdError e == StdError e' = e == e'
  e@(RichError _ _ hds) == e'@(RichError _ _ hds') = toJSON e == toJSON e' && hds == hds'
  StdError {} == RichError {} = False
  RichError {} == StdError {} = False

instance Show HttpError where
  show (StdError werr) = "StdError (" <> show werr <> ")"
  show e@(RichError _ _ headers) = "RichError (json = " <> Text.unpack (Text.decodeUtf8 $ BS.toStrict $ encode e) <> ", headers = " <> show headers <> ")"

instance Exception HttpError

httpErrorToWaiError :: HttpError -> Wai.Error
httpErrorToWaiError (StdError e) = e
httpErrorToWaiError (RichError e _ _) = e

errorLabel :: HttpError -> LText
errorLabel = (.label) . httpErrorToWaiError

instance ToJSON HttpError where
  toJSON (StdError e) = toJSON e
  toJSON (RichError e x _) = case (toJSON e, toJSON x) of
    (Object o1, Object o2) -> Object (KeyMap.union o1 o2)
    (j, _) -> j

httpErrorToJSONResponse :: HttpError -> JSONResponse
httpErrorToJSONResponse (StdError werr) = waiErrorToJSONResponse werr
httpErrorToJSONResponse e@(RichError werr _ headers) =
  JSONResponse
    { status = werr.code,
      value = toJSON e,
      headers = headers
    }

postgresUsageErrorToHttpError :: UsageError -> HttpError
postgresUsageErrorToHttpError err = case err of
  SessionUsageError _se ->
    -- FUTUREWORK: should this case should be more nuanced?  eg., if a foreign key is dangling, should we
    -- return "404 not found", not "database crashed"?
    -- The problem is that the SessionError is not typed to easily be parsed
    -- To prevent foreign key errors we should check the foreign key constraints before inserting
    StdError (Wai.mkError status500 "server-error" (LT.pack $ "postgres: " <> show err))
  ConnectionUsageError _ -> StdError (Wai.mkError status500 "server-error" (LT.pack $ "postgres: " <> show err))
  AcquisitionTimeoutUsageError -> StdError (Wai.mkError status500 "server-error" (LT.pack $ "postgres: " <> show err))

-- | Extract the wai error from an HttpError and convert into a
-- servant error.  `RichError` extra data is discarded!
httpErrorToServerError :: HttpError -> ServerError
httpErrorToServerError err =
  ServerError
    (statusCode (httpErrorToWaiError err).code)
    (UTF8BS.toString $ statusMessage $ (httpErrorToWaiError err).code)
    ( if (httpErrorToWaiError err).label == "unknown-error"
        then LText.encodeUtf8 (httpErrorToWaiError err).message
        else encode err
    )
    []

-- | Construct a StdError from a servant error.
serverErrorToHttpError :: ServerError -> HttpError
serverErrorToHttpError ServerError {..} =
  StdError $
    Wai.mkError
      (Status errHTTPCode (UTF8BS.fromString errReasonPhrase))
      lbl
      msg
  where
    (lbl, msg) = case decode @Wai.Error errBody of
      Just err -> (err.label :: LText, err.message :: LText)
      Nothing -> ("unknown-error", decodeUtf8With lenientDecode errBody) -- just make something up.
