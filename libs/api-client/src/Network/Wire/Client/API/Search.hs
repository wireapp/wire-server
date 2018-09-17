{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.Search
    ( Contact      (..)
    , SearchResult (..)
    , search
    ) where

import Bilge
import Brig.Types
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Int
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session
import Named

import qualified Data.ByteString.Char8 as C

-- | Search for already connected and/or potential contacts.
search
    :: MonadSession m
    => "query" :! Text
    -> "limit" :? Int32
    -> m (SearchResult Contact)
search
    (arg #query -> searchQuery)
    (argDef #limit 15 -> searchLimit)
    =
    sessionRequest req rsc readBody
  where
    req = method GET
        . path "/search/contacts"
        . acceptJson
        . queryItem "q" (encodeUtf8 searchQuery)
        . queryItem "size" (C.pack $ show searchLimit)
        $ empty
    rsc = status200 :| []
