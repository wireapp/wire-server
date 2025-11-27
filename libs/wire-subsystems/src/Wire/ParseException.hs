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
