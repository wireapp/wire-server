{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings      #-}

module Web.SCIM.Schema.User where

import Prelude hiding (map)

import Data.Text (Text)
import Data.Aeson

import Web.SCIM.Schema.Common
import Web.SCIM.Schema.Schema (Schema)
import Web.SCIM.Schema.User.Address (Address)
import Web.SCIM.Schema.User.Certificate (Certificate)
import Web.SCIM.Schema.User.Email (Email)
import Web.SCIM.Schema.User.IM (IM)
import Web.SCIM.Schema.User.Name (Name)
import Web.SCIM.Schema.User.Phone (Phone)
import Web.SCIM.Schema.User.Photo (Photo)

import GHC.Generics (Generic)


data User = User
  { schemas :: [Schema]
  , userName :: Text
  , externalId :: Maybe Text
  , name :: Maybe Name
  , displayName :: Maybe Text
  , nickName :: Maybe Text
  , profileUrl :: Maybe URI
  , title :: Maybe Text
  , userType :: Maybe Text
  , preferredLanguage :: Maybe Text
  , locale :: Maybe Text
  , active :: Maybe Bool
  , password :: Maybe Text
  , emails :: Maybe [Email]
  , phoneNumbers :: Maybe [Phone]
  , ims :: Maybe [IM]
  , photos :: Maybe [Photo]
  , addresses :: Maybe [Address]
  , entitlements :: Maybe [Text]
  , roles :: Maybe [Text]
  , x509Certificates :: Maybe [Certificate]
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON User where
  toJSON = genericToJSON serializeOptions

-- TODO: type parameter?
type UserId = Text

