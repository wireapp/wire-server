{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings      #-}

module Web.SCIM.Schema.Error (notFound, fromErrorType) where

import Data.Text (Text, pack)
import Data.Aeson hiding (Error)
import Data.Monoid ((<>))
import Web.SCIM.Schema.Common
import Servant (ServantErr (..), err404, err400)

import GHC.Generics (Generic)

data ErrorType = InvalidFilter
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

instance ToJSON ErrorType where
  toJSON InvalidFilter = "invalidFilter"
  toJSON TooMany       = "tooMany"
  toJSON Uniqueness    = "uniqueness"
  toJSON Mutability    = "mutability"
  toJSON InvalidSyntax = "invalidSyntax"
  toJSON InvalidPath   = "invalidPath"
  toJSON NoTarget      = "noTarget"
  toJSON InvalidValue  = "invalidValue"
  toJSON InvalidVers   = "invalidVers"
  toJSON Sensitive     = "sensitive"


fromErrorType :: ErrorType -> ServantErr
fromErrorType typ =
  let body = Error
        { schemas = [Error2_0]
        , status = Status 400
        , scimType = pure typ
        , detail = Nothing
        }
  in err400 { errBody = encode body
            , errHeaders = [("Content-Type", "application/json")]
            }

notFound :: Text -> ServantErr
notFound rid =
  let body = Error
        { schemas = [Error2_0]
        , status = Status 404
        , scimType = Nothing
        , detail = pure $ "Resource " <> rid <> " not found"
        }
  in err404 { errBody = encode body
            , errHeaders = [("Content-Type", "application/json")]
            }

-- wrapped in a newtype because SCIM wants strings for status codes
newtype Status = Status Int
  deriving (Show, Eq, Generic)

instance ToJSON Status where
  toJSON (Status stat) = String . pack . show $ stat

-- TODO: move to common schema datatype
data Schema = Error2_0
  deriving (Show, Eq)

instance ToJSON Schema where
  toJSON Error2_0 = String "urn:ietf:params:scim:api:messages:2.0:Error"

data Error = Error
  { schemas :: [Schema]
  , status :: Status
  , scimType :: Maybe ErrorType
  , detail :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Error where
  toJSON = genericToJSON serializeOptions
