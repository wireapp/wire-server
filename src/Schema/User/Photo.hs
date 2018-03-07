{-# LANGUAGE DeriveGeneric      #-}

module Schema.User.Photo where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

import Schema.Common

data Photo = Photo
  { typ :: Maybe Text
  , value :: Maybe URI
  } deriving (Show, Eq, Generic)

instance FromJSON Photo where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Photo where
  toJSON = genericToJSON serializeOptions
