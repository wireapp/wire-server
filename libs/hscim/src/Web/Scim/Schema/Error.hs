-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

-- | SCIM errors
module Web.Scim.Schema.Error
  ( -- * Types
    ScimErrorType (..),
    ScimError (..),
    Status (..),

    -- * Constructors
    NotFound (..),
    BadRequest (..),
    UnAuthorized (..),
    Forbidden (..),
    Conflict (..),

    -- * Servant interoperability
    scimToServerError,
  )
where

import Data.Aeson hiding (Error)
import Data.Kind (Constraint)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Servant (HasStatus (..), ServerError (..))
import Servant.API.UVerb (All, Union, foldMapUnion)
import Web.Scim.Schema.Common
import Web.Scim.Schema.Schema

----------------------------------------------------------------------------
-- Types

-- | An ADT for error types in the SCIM specification. Not all possible SCIM
-- errors have a corresponding 'ScimErrorType' (for instance, authorization
-- is not covered by this type).
--
-- See <https://tools.ietf.org/html/rfc7644#page-69>
data ScimErrorType
  = InvalidFilter
  | TooMany
  | Uniqueness
  | Mutability
  | InvalidSyntax
  | InvalidPath
  | NoTarget
  | InvalidValue
  | InvalidVers
  | Sensitive
  deriving (Show, Eq, Generic)

instance ToJSON ScimErrorType where
  toJSON InvalidFilter = "invalidFilter"
  toJSON TooMany = "tooMany"
  toJSON Uniqueness = "uniqueness"
  toJSON Mutability = "mutability"
  toJSON InvalidSyntax = "invalidSyntax"
  toJSON InvalidPath = "invalidPath"
  toJSON NoTarget = "noTarget"
  toJSON InvalidValue = "invalidValue"
  toJSON InvalidVers = "invalidVers"
  toJSON Sensitive = "sensitive"

-- wrapped in a newtype because SCIM wants strings for status codes
newtype Status = Status {unStatus :: Int}
  deriving (Show, Eq, Generic)

instance ToJSON Status where
  toJSON (Status stat) = String . pack . show $ stat

data ScimError = ScimError
  { schemas :: [Schema],
    status :: Status,
    scimType :: Maybe ScimErrorType,
    detail :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ScimError where
  toJSON = genericToJSON serializeOptions

----------------------------------------------------------------------------
-- Exception constructors [deprecated]

data BadRequest = BadRequest
  { badRequestErrorType :: ScimErrorType,
    badRequestDetails :: Maybe Text
  }

instance HasStatus BadRequest where
  type StatusOf BadRequest = 400

instance ToJSON BadRequest where
  toJSON (BadRequest t d) =
    toJSON $
      ScimError
        { schemas = [Error20],
          status = Status 400,
          scimType = pure t,
          detail = d
        }

data UnAuthorized = UnAuthorized
  { unAuthorizedDetails :: Text
  }

instance HasStatus UnAuthorized where
  type StatusOf UnAuthorized = 401

instance ToJSON UnAuthorized where
  toJSON (UnAuthorized details) =
    toJSON $
      ScimError
        { schemas = [Error20],
          status = Status 401,
          scimType = Nothing,
          detail = pure $ "authorization failed: " <> details
        }

data Forbidden = Forbidden
  {forbiddenDetails :: Text}

instance HasStatus Forbidden where
  type StatusOf Forbidden = 403

instance ToJSON Forbidden where
  toJSON (Forbidden details) =
    toJSON $
      ScimError
        { schemas = [Error20],
          status = Status 403,
          scimType = Nothing,
          detail = pure $ "forbidden: " <> details
        }

data NotFound = NotFound
  { notFoundResourceType :: Text,
    notFoundResourceId :: Text
  }

instance HasStatus NotFound where
  type StatusOf NotFound = 404

instance ToJSON NotFound where
  toJSON (NotFound resourceType resourceId) =
    toJSON $
      ScimError
        { schemas = [Error20],
          status = Status 404,
          scimType = Nothing,
          detail = pure $ resourceType <> " " <> resourceId <> " not found"
        }

data Conflict = Conflict

instance HasStatus Conflict where
  type StatusOf Conflict = 409

instance ToJSON Conflict where
  toJSON Conflict =
    toJSON $
      ScimError
        { schemas = [Error20],
          status = Status 409,
          scimType = Just Uniqueness,
          detail = Nothing
        }

----------------------------------------------------------------------------
-- Servant

type HasStatusJSON a = (HasStatus a, ToJSON a) :: Constraint

toServerError :: forall (as :: [*]). All HasStatusJSON as => Union as -> Maybe ServerError
toServerError = undefined

-- | Convert a SCIM 'Error' to a Servant one by encoding it with the
-- appropriate headers.
scimToServerError :: ScimError -> ServerError
scimToServerError err =
  ServerError
    { errHTTPCode = unStatus (status err),
      errReasonPhrase = reasonPhrase (status err),
      errBody = encode err,
      errHeaders = [("Content-Type", "application/scim+json;charset=utf-8")]
    }

-- | A mapping of error code "reason phrases" (e.g. "Method Not Allowed")
-- for all 4xx and 5xx errors.
reasonPhrase :: Status -> String
reasonPhrase = \case
  Status 400 -> "Bad Request"
  Status 401 -> "Unauthorized"
  Status 402 -> "Payment Required"
  Status 403 -> "Forbidden"
  Status 404 -> "Not Found"
  Status 405 -> "Method Not Allowed"
  Status 406 -> "Not Acceptable"
  Status 407 -> "Proxy Authentication Required"
  Status 408 -> "Request Time-out"
  Status 409 -> "Conflict"
  Status 410 -> "Gone"
  Status 411 -> "Length Required"
  Status 412 -> "Precondition Failed"
  Status 413 -> "Request Entity Too Large"
  Status 414 -> "Request-URI Too Large"
  Status 415 -> "Unsupported Media Type"
  Status 416 -> "Range Not Satisfiable"
  Status 417 -> "Expectation Failed"
  Status 422 -> "Unprocessable Entity"
  Status 500 -> "Internal Server Error"
  Status 501 -> "Not Implemented"
  Status 502 -> "Bad Gateway"
  Status 503 -> "Service Unavailable"
  Status 504 -> "Gateway Time-out"
  Status 505 -> "HTTP Version not supported"
  other -> show other
