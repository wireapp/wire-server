-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Network.Wai.Utilities.JSONResponse
  ( JSONResponse (..),
    waiErrorToJSONResponse,
    jsonResponseToWai,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.Response

-- | A custom JSON response to be returned to the client as an error.
--
-- Both static and dynamic errors are converted to this type before being
-- turned into HTTP responses. It is a generalisation of 'Wai.Error',
-- encompassing both standard error values (including @label@, @code@ and
-- @message@ fields), and custom error-like responses that include extra data
-- for the clients.
data JSONResponse = JSONResponse
  { status :: Status,
    value :: A.Value,
    headers :: [Header]
  }
  deriving (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema JSONResponse

instance ToSchema JSONResponse where
  schema =
    object "JSONResponse" $
      JSONResponse
        <$> status .= field "status" (toEnum <$> (fromEnum .= schema))
        <*> value .= field "value" jsonValue
        <*> headers .= pure []

instance Exception JSONResponse

waiErrorToJSONResponse :: Wai.Error -> JSONResponse
waiErrorToJSONResponse e =
  JSONResponse
    { status = Wai.code e,
      value = toJSON e,
      headers = []
    }

jsonResponseToWai :: JSONResponse -> Response
jsonResponseToWai r = responseLBS r.status (jsonContent : r.headers) (A.encode r.value)
