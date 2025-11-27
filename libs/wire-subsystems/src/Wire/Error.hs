{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
import Network.Wai
import Network.Wai.Utilities
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.JSONResponse
import Network.Wai.Utilities.Server
import Servant (ServerError (..))
import System.Logger qualified as Log

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
    ( case err of
        StdError _ -> []
        RichError _ _ hds -> hds
    )

-- | Construct a StdError from a servant error.
serverErrorToHttpError :: ServerError -> HttpError
serverErrorToHttpError ServerError {..} = case errHeaders of
  [] ->
    StdError waierr
  hds@(_ : _) ->
    RichError waierr Null hds
  where
    waierr =
      Wai.mkError
        (Status errHTTPCode (UTF8BS.fromString errReasonPhrase))
        lbl
        msg
    (lbl, msg) = case decode @Wai.Error errBody of
      Just err -> (err.label :: LText, err.message :: LText)
      Nothing -> ("unknown-error", decodeUtf8With lenientDecode errBody)

logHttpError :: (MonadIO m) => Log.Logger -> Maybe Request -> HttpError -> m ()
logHttpError l mr = logHttpError' l (lookupRequestId defaultRequestIdHeaderName =<< mr)

logHttpError' :: (MonadIO m) => Log.Logger -> Maybe ByteString -> HttpError -> m ()
logHttpError' l mr (httpErrorToWaiError -> e) = liftIO $ doLog l (logErrorMsgWithRequest mr e)
  where
    doLog
      | statusCode (Wai.code e) >= 500 = Log.err
      | otherwise = Log.debug
