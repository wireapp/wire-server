{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wire.Client.API.Search
    ( Contact      (..)
    , SearchParams (..)
    , SearchResult (..)
    , search
    ) where

import Imports
import Bilge
import Brig.Types
import Control.Monad.Catch (MonadMask)
import Data.Default.Class
import Data.List.NonEmpty
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session

import qualified Data.ByteString.Char8 as C

data SearchParams = SearchParams
    { searchText      :: !Text
    , searchDistance  :: !Word8
    , searchLimit     :: !Word8
    , searchDirectory :: !Bool
    } deriving (Show)

instance Default SearchParams where
    def = SearchParams "" 2 10 True

-- | Search for already connected and/or potential contacts.
search :: (MonadSession m, MonadUnliftIO m, MonadMask m) => SearchParams -> m (SearchResult Contact)
search SearchParams{..} = sessionRequest req rsc readBody
  where
    req = method GET
        . path "/search/contacts"
        . acceptJson
        . queryItem "q" (encodeUtf8 searchText)
        . queryItem "l" (C.pack $ show searchDistance)
        . queryItem "size" (C.pack $ show searchLimit)
        . queryItem "d" (if searchDirectory then "1" else "0")
        $ empty
    rsc = status200 :| []
