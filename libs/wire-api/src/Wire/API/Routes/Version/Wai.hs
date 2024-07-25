-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Version.Wai (versionMiddleware) where

import Control.Monad.Except (throwError)
import Data.ByteString.Conversion
import Data.EitherR (fmapL)
import Data.Text qualified as T
import Data.Text.Lazy (fromStrict)
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.Wai
import Network.Wai.Middleware.Rewrite
import Network.Wai.Utilities.Error
import Network.Wai.Utilities.Response
import Web.HttpApiData (parseUrlPiece, toUrlPiece)
import Wire.API.Routes.Version

-- | Strip off version prefix. Return 404 if the version is not supported.
versionMiddleware :: Set Version -> Middleware
versionMiddleware disabledAPIVersions app req k = case parseVersion (removeVersionHeader req) of
  Right (req', v) -> do
    if v `elem` disabledAPIVersions && requestIsDisableable req'
      then err (toUrlPiece v)
      else app (addVersionHeader v req') k
  Left (BadVersion v) -> err v
  Left NoVersion -> app req k
  Left InternalApisAreUnversioned -> errint
  where
    err :: Text -> IO ResponseReceived
    err v =
      k . errorRs . mkError HTTP.status404 "unsupported-version" $
        "Version " <> fromStrict v <> " is not supported"

    errint :: IO ResponseReceived
    errint =
      k . errorRs . mkError HTTP.status404 "unsupported-version" $
        "Internal APIs (`/i/...`) are not under version control"

data ParseVersionError = NoVersion | BadVersion Text | InternalApisAreUnversioned

parseVersion :: Request -> Either ParseVersionError (Request, Version)
parseVersion req = do
  (version, pinfo) <- case pathInfo req of
    [] -> throwError NoVersion
    (x : xs) -> do
      unless (looksLikeVersion x) $
        throwError NoVersion
      case xs of
        ("i" : _) -> throwError InternalApisAreUnversioned
        ("api-internal" : _) -> throwError InternalApisAreUnversioned
        _ -> pure (x, xs)
  n <- fmapL (const $ BadVersion version) $ parseUrlPiece version
  pure (rewriteRequestPure (\(_, q) _ -> (pinfo, q)) req, n)

looksLikeVersion :: Text -> Bool
looksLikeVersion version = case T.splitAt 1 version of (h, t) -> h == "v" && T.all isDigit t

-- | swagger-delivering end-points are not disableable: they should work for all versions.
requestIsDisableable :: Request -> Bool
requestIsDisableable (pathInfo -> path) = case path of
  ["api", "swagger-ui"] -> False
  ["api", "swagger.json"] -> False
  _ -> True

removeVersionHeader :: Request -> Request
removeVersionHeader req =
  req
    { requestHeaders = filter ((/= versionHeader) . fst) (requestHeaders req)
    }

addVersionHeader :: Version -> Request -> Request
addVersionHeader v req =
  req
    { requestHeaders = (versionHeader, toByteString' (versionInt v :: Int)) : requestHeaders req
    }
