{-# LANGUAGE RecordWildCards #-}

module Web.Scim.Schema.ListResponse
  ( ListResponse (..),
    fromList,
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Web.Scim.Schema.Common
import Web.Scim.Schema.Schema

-- | A "pagination" type used as a wrapper whenever a SCIM endpoint has to
-- return a list.
--
-- Pagination is not actually supported anywhere in the code yet; whenever
-- there are several results we always return them all as one page, and we
-- don't support different values of 'startIndex'.
--
-- FUTUREWORK: Support for pagination might be added once we have to handle
-- organizations with lots of users.
data ListResponse a
  = ListResponse
      { schemas :: [Schema],
        totalResults :: Int,
        itemsPerPage :: Int,
        startIndex :: Int,
        resources :: [a]
      }
  deriving (Show, Eq, Generic)

fromList :: [a] -> ListResponse a
fromList list =
  ListResponse
    { schemas = [ListResponse20],
      totalResults = len,
      itemsPerPage = len,
      startIndex = 1, -- NOTE: lists are 1-indexed in SCIM
      resources = list
    }
  where
    len = length list

instance FromJSON a => FromJSON (ListResponse a) where
  parseJSON = genericParseJSON parseOptions . jsonLower

instance ToJSON a => ToJSON (ListResponse a) where
  toJSON ListResponse {..} =
    object
      [ "Resources" .= resources,
        "schemas" .= schemas,
        "totalResults" .= totalResults,
        "itemsPerPage" .= itemsPerPage,
        "startIndex" .= startIndex
      ]
