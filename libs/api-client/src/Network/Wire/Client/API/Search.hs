{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module Network.Wire.Client.API.Search
  ( Contact (..),
    SearchParams (..),
    SearchResult (..),
    search,
  )
where

import Bilge
import Brig.Types (Contact (..), SearchResult (..))
import Control.Monad.Catch (MonadMask)
import qualified Data.ByteString.Char8 as C
import Data.Default.Class
import Data.List.NonEmpty
import Data.Text.Encoding (encodeUtf8)
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session

data SearchParams = SearchParams
  { searchText :: !Text,
    searchDistance :: !Word8,
    searchLimit :: !Word8,
    searchDirectory :: !Bool
  }
  deriving (Show)

instance Default SearchParams where
  def = SearchParams "" 2 10 True

-- | Search for already connected and/or potential contacts.
search :: (MonadSession m, MonadUnliftIO m, MonadMask m) => SearchParams -> m (SearchResult Contact)
search SearchParams {..} = sessionRequest req rsc readBody
  where
    req =
      method GET
        . path "/search/contacts"
        . acceptJson
        . queryItem "q" (encodeUtf8 searchText)
        . queryItem "l" (C.pack $ show searchDistance)
        . queryItem "size" (C.pack $ show searchLimit)
        . queryItem "d" (if searchDirectory then "1" else "0")
        $ empty
    rsc = status200 :| []
