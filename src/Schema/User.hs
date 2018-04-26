{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings      #-}

module Schema.User where

import Prelude hiding (map)

import Data.Text (Text)
import Data.Aeson

import Schema.Common
import Schema.Schema (Schema)
import Schema.User.Address (Address)
import Schema.User.Certificate (Certificate)
import Schema.User.Email (Email)
import Schema.User.IM (IM)
import Schema.User.Name (Name)
import Schema.User.Phone (Phone)
import Schema.User.Photo (Photo)

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

