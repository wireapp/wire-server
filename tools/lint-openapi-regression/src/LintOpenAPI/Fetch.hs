module LintOpenAPI.Fetch (fetchVersionSpec) where

import Data.Text qualified as Text
import Imports
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import UnliftIO.Exception (try)

-- | Fetch the OpenAPI spec that a running brig serves for API version @n@:
-- @GET {url}/v{n}/api/swagger.json@.  Non-2xx responses yield @Left@.
fetchVersionSpec :: Text -> Int -> IO (Either String LByteString)
fetchVersionSpec url n = do
  let fullUrl = url <> "/v" <> Text.pack (show n) <> "/api/swagger.json"
  manager <- HC.newManager tlsManagerSettings
  eResp <-
    try (HC.parseRequest (Text.unpack fullUrl) >>= \req -> HC.httpLbs req {HC.redirectCount = 0} manager) ::
      IO (Either HC.HttpException (HC.Response LByteString))
  pure $ case eResp of
    Left err -> Left ("GET " <> Text.unpack fullUrl <> " failed: " <> show err)
    Right resp
      | statusCode (HC.responseStatus resp) `div` 100 == 2 ->
          Right (HC.responseBody resp)
      | otherwise ->
          Left
            ( "GET "
                <> Text.unpack fullUrl
                <> " failed: HTTP "
                <> show (statusCode (HC.responseStatus resp))
            )
