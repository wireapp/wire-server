{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedStrings      #-}

module Schema.ListResponse (ListResponse, fromList) where

import Data.Aeson
import GHC.Generics (Generic)
import Schema.Common
import Schema.Schema

data ListResponse a = ListResponse
  { schemas :: [Schema]
  , totalResults :: Int
  , resources :: [a]
  } deriving (Show, Eq, Generic)

fromList :: [a] -> ListResponse a
fromList list = ListResponse
  { schemas = [ListResponse2_0]
  , totalResults = length list
  , resources = list
  }

instance FromJSON a => FromJSON (ListResponse a) where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON a => ToJSON (ListResponse a) where
  toJSON = genericToJSON serializeOptions

