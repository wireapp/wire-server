{-# LANGUAGE DeriveGeneric      #-}

module Web.SCIM.Schema.User.Certificate where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

import Web.SCIM.Schema.Common


data Certificate = Certificate
  { typ :: Maybe Text
  , value :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON Certificate where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON Certificate where
  toJSON = genericToJSON serializeOptions
