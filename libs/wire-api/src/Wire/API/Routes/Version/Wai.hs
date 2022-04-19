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

module Wire.API.Routes.Version.Wai where

import Data.ByteString.Conversion
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Lazy as LText
import Imports
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import Network.Wai.Middleware.Rewrite
import Network.Wai.Utilities.Error
import Network.Wai.Utilities.Response
import Wire.API.Routes.Version

versionHeader :: CI.CI ByteString
versionHeader = "X-Wire-API-Version"

-- | Strip off version prefix. Return 404 if the version is not supported.
versionMiddleware :: Middleware
versionMiddleware app req k = case parseVersion (removeVersionHeader req) of
  Nothing -> app req k
  Just (req', n) -> case mkVersion n of
    Just v -> app (addVersionHeader v req') k
    Nothing ->
      k $
        errorRs' $
          mkError HTTP.status404 "unsupported-version" $
            "Version " <> LText.pack (show n) <> " is not supported"

parseVersion :: Request -> Maybe (Request, Integer)
parseVersion req = do
  (version, pinfo) <- case pathInfo req of
    [] -> Nothing
    (x : xs) -> pure (x, xs)
  n <- readVersionNumber version
  pure (rewriteRequestPure (\(_, q) _ -> (pinfo, q)) req, n)

removeVersionHeader :: Request -> Request
removeVersionHeader req =
  req
    { requestHeaders = filter ((/= versionHeader) . fst) (requestHeaders req)
    }

addVersionHeader :: Version -> Request -> Request
addVersionHeader v req =
  req
    { requestHeaders = (versionHeader, toByteString' (fromEnum v)) : requestHeaders req
    }
