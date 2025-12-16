{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Schema.ListResponse
  ( ListResponse (..),
    fromList,
    toPage,
  )
where

import Data.Aeson
import Data.Int (Int32)
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
data ListResponse a = ListResponse
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

toPage :: Int32 -> Maybe Int32 -> [a] -> ListResponse a
toPage startIndex mbCount list = case mbCount of
  Nothing ->
    ListResponse
      { schemas = [ListResponse20],
        totalResults = totalResults',
        startIndex = startIndex',
        itemsPerPage = length list',
        resources = list'
      }
  Just count ->
    let (c, page, _rest) = splitAtCount safeCount list'
        safeCount = max 0 (min count (maxBound @Int32))
     in ListResponse
          { schemas = [ListResponse20],
            totalResults = totalResults',
            startIndex = startIndex',
            itemsPerPage = fromIntegral c,
            resources = page
          }
  where
    totalResults' = length list
    startIndex' = max (fromIntegral startIndex) 1
    list' = drop (startIndex' - 1) list

-- | Split @list@ at @n@, while returning the count of elements
-- actually in the prefix (as it can be shorter than @n@).
splitAtCount :: Int32 -> [a] -> (Int32, [a], [a])
splitAtCount n list = go n 0 list
  where
    go :: Int32 -> Int32 -> [a] -> (Int32, [a], [a])
    go _ c [] = (c, [], [])
    go 0 c rest = (c, [], rest)
    go n' c (x : xs) =
      let (c', xs', rest) = go (n' - 1) (c + 1) xs
       in (c', x : xs', rest)

instance (FromJSON a) => FromJSON (ListResponse a) where
  parseJSON = either (fail . show) (genericParseJSON parseOptions) . jsonLower

instance (ToJSON a) => ToJSON (ListResponse a) where
  toJSON ListResponse {..} =
    object
      [ "Resources" .= resources,
        "schemas" .= schemas,
        "totalResults" .= totalResults,
        "itemsPerPage" .= itemsPerPage,
        "startIndex" .= startIndex
      ]
