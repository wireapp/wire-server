{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Brig.User.API.Search (routes) where

import Imports
import Brig.API.Handler
import Brig.User.Event
import Brig.User.Search.Index
import Data.Id
import Data.Range
import Data.Predicate
import Network.HTTP.Types.Status
import Network.Wai (Request, Response)
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities.Response (json, empty, setStatus)
import Network.Wai.Utilities.Swagger (document)

import qualified Brig.Data.User         as DB
import qualified Brig.IO.Intra          as Intra
import qualified Data.Swagger.Build.Api as Doc
import qualified Brig.Types.Swagger     as Doc

routes :: Routes Doc.ApiBuilder Handler ()
routes = do
    get "/search/contacts" (continue search) $
        accept "application" "json"
        .&. header "Z-User"
        .&. query "q"
        .&. def (unsafeRange 15) (query "size")

    document "GET" "search" $ do
        Doc.summary "Search for users"
        Doc.parameter Doc.Query "q" Doc.string' $
            Doc.description "Search query"
        Doc.parameter Doc.Query "size" Doc.int32' $ do
            Doc.description "Number of results to return"
            Doc.optional
        Doc.returns (Doc.ref Doc.searchResult)
        Doc.response 200 "The search result." Doc.end

    --

    get "/self/searchable" (continue isSearchable) $
        accept "application" "json"
        .&. header "Z-User"

    document "GET" "getSearchableStatus" $ do
        Doc.summary "Determine whether you are disoverable via /search/contacts."
        Doc.returns (Doc.ref Doc.searchableStatus)
        Doc.response 200 "Searchable status." Doc.end

    --

    put "/self/searchable" (continue setSearchable) $
        contentType "application" "json"
        .&. header "Z-User"
        .&. request

    document "PUT" "updateSearchableStatus" $ do
        Doc.summary "Opt in or out of being included in search results."
        Doc.body (Doc.ref Doc.searchableStatus) $
            Doc.description "JSON body"
        Doc.response 200 "Searchable status updated." Doc.end

    --

    -- make index updates visible (e.g. for integration testing)
    post "/i/index/refresh"
        (continue (const $ lift refreshIndex *> pure empty))
        true

    -- reindex from Cassandra (e.g. integration testing -- prefer the
    -- `brig-index` executable for actual operations!)
    post "/i/index/reindex"
        (continue . const $ lift reindexAll *> pure empty)
        true

-- Handlers

search :: JSON ::: UserId ::: Text ::: Range 1 100 Int32 -> Handler Response
search (_ ::: u ::: q ::: s) = json <$> lift (searchIndex u q s)

isSearchable :: JSON ::: UserId -> Handler Response
isSearchable (_ ::: u) = json <$> lift (checkIndex u)

setSearchable :: JSON ::: UserId ::: Request-> Handler Response
setSearchable (_ ::: u ::: r) = do
    s <- parseJsonBody r
    lift $ DB.updateSearchableStatus u s
    lift $ Intra.onUserEvent u Nothing (searchableStatusUpdated u s)
    return (setStatus status200 empty)
