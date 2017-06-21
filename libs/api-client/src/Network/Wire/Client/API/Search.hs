{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wire.Client.API.Search
    ( Contact      (..)
    , SearchParams (..)
    , SearchResult (..)
    , search
    ) where

import Bilge
import Brig.Types
import Control.Monad (mfilter)
import Data.Aeson
import Data.Default.Class
import Data.Id
import Data.Int
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

data SearchParams = SearchParams
    { searchText      :: Text
    , searchDistance  :: Word8
    , searchLimit     :: Word8
    , searchDirectory :: Bool
    } deriving (Show)

instance Default SearchParams where
    def = SearchParams "" 2 10 True

data Contact = Contact
    { contactId        :: UserId
    , contactName      :: Name
    , contactPhone     :: Maybe Phone
    , contactEmail     :: Maybe Email
    , contactDistance  :: Maybe Int32
    , contactWeight    :: Maybe Int16
    , contactBlocked   :: Maybe Bool
    , contactConnected :: Maybe Bool
    } deriving (Show)

data SearchResult = SearchResult
    { searchFound    :: Int32
    , searchReturned :: Int32
    , searchResults  :: [Contact]
    } deriving (Show)

-- | Search for already connected and/or potential contacts.
search :: MonadSession m => SearchParams -> m SearchResult
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

-- JSON mapping

instance FromJSON SearchResult where
    parseJSON = withObject "search-result" $ \o ->
        SearchResult <$> o .: "found"
                     <*> o .: "returned"
                     <*> o .: "documents"

instance FromJSON Contact where
    parseJSON = withObject "search-user" $ \o ->
        Contact <$> o .:  "id"
                <*> (Name <$> o .: "name")
                <*> (fmap Phone . mfilter T.null <$> o .:? "phone")
                <*> o .:? "email"
                <*> o .:? "level"
                <*> o .:? "weight"
                <*> o .:? "blocked"
                <*> o .:? "connected"

