{-# LANGUAGE OverloadedStrings #-}

-- | Bindings to "Brig.User.API.Search".
module Network.Wire.Client.API.Search
    (
    -- * Types and constants
      Contact (..)
    , SearchResult (..)
    , SearchableStatus (..)
    , optIn, optOut

    -- * Public endpoints
    , search
    , isSearchable
    , setSearchable

    -- * Internal endpoints
    , refreshIndex
    , reindexAll
    ) where

import Bilge
import Brig.Types
import Data.List.NonEmpty
import Data.Aeson (encode)
import Data.Text (Text)
import Data.Int
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (status200)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session
import Network.Wire.Client.Monad
import GHC.Stack (HasCallStack)
import Named

import qualified Data.ByteString.Char8 as C

----------------------------------------------------------------------------
-- Types and constants

optIn, optOut :: SearchableStatus
optIn  = SearchableStatus True
optOut = SearchableStatus False

----------------------------------------------------------------------------
-- Public endpoints

-- | Search for already connected and/or potential contacts.
search
    :: (HasCallStack, MonadSession m)
    => "query" :! Text
    -> "limit" :? Int32
    -> m (SearchResult Contact)
search
    (arg #query -> searchQuery)
    (argDef #limit 15 -> searchLimit)
    =
    sessionRequest Brig req rsc readBody
  where
    req = method GET
        . path "/search/contacts"
        . acceptJson
        . queryItem "q" (encodeUtf8 searchQuery)
        . queryItem "size" (C.pack $ show searchLimit)
        $ empty
    rsc = status200 :| []

-- | Determine whether you are discoverable via @\/search\/contacts@.
isSearchable
    :: (HasCallStack, MonadSession m)
    => m SearchableStatus
isSearchable
    = sessionRequest Brig req rsc readBody
  where
    req = method GET
        . path "/self/searchable"
        $ empty
    rsc = status200 :| []

-- | Opt in or out of being included in search results.
setSearchable
    :: (HasCallStack, MonadSession m)
    => SearchableStatus
    -> m ()
setSearchable status
    = sessionRequest Brig req rsc (const $ return ())
  where
    req = method PUT
        . path "/self/searchable"
        . contentJson
        . body (RequestBodyLBS (encode status))
        $ empty
    rsc = status200 :| []

----------------------------------------------------------------------------
-- Internal endpoints

-- | Make index updates visible.
refreshIndex
    :: (HasCallStack, MonadSession m)
    => m ()
refreshIndex
    = sessionRequest Brig req rsc (const $ return ())
  where
    req = method POST
        . path "/i/index/refresh"
        $ empty
    rsc = status200 :| []

-- | Reindex from Cassandra.
reindexAll
    :: (HasCallStack, MonadSession m)
    => m ()
reindexAll
    = sessionRequest Brig req rsc (const $ return ())
  where
    req = method POST
        . path "/i/index/reindex"
        $ empty
    rsc = status200 :| []
