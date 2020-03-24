module Brig.User.API.Search (routes) where

import Brig.API.Handler
import Brig.App
import qualified Brig.Data.User as DB
import qualified Brig.IO.Intra as Intra
import Brig.Options
import Brig.Types.Search hiding (isSearchable)
import qualified Brig.Types.Swagger as Doc
import Brig.User.Event
import Brig.User.Search.Index
import Control.Lens
import Data.Id
import Data.Predicate
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities.Request (JsonRequest, jsonRequest)
import Network.Wai.Utilities.Response (empty, json, setStatus)
import Network.Wai.Utilities.Swagger (document)

routes :: Routes Doc.ApiBuilder Handler ()
routes = do
  get "/search/contacts" (continue searchH) $
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

  get "/self/searchable" (continue isSearchableH) $
    accept "application" "json"
      .&. header "Z-User"
  document "GET" "getSearchableStatus" $ do
    Doc.summary "Determine whether you are disoverable via /search/contacts."
    Doc.returns (Doc.ref Doc.searchableStatus)
    Doc.response 200 "Searchable status." Doc.end
  --

  -- UserUpdated event to contacts
  put "/self/searchable" (continue setSearchableH) $
    header "Z-User"
      .&. jsonRequest @SearchableStatus
  document "PUT" "updateSearchableStatus" $ do
    Doc.summary "Opt in or out of being included in search results."
    Doc.body (Doc.ref Doc.searchableStatus) $
      Doc.description "JSON body"
    Doc.response 200 "Searchable status updated." Doc.end
  --

  -- make index updates visible (e.g. for integration testing)
  post
    "/i/index/refresh"
    (continue (const $ lift refreshIndex *> pure empty))
    true
  -- reindex from Cassandra (e.g. integration testing -- prefer the
  -- `brig-index` executable for actual operations!)
  post
    "/i/index/reindex"
    (continue . const $ lift reindexAll *> pure empty)
    true

-- Handlers

searchH :: JSON ::: UserId ::: Text ::: Range 1 100 Int32 -> Handler Response
searchH (_ ::: u ::: q ::: s) = json <$> lift (search u q s)

search :: UserId -> Text -> Range 1 100 Int32 -> AppIO (SearchResult Contact)
search u q s = do
  sameTeamSearchOnly <- fromMaybe False <$> view (settings . searchSameTeamOnly)
  contacts <- searchIndex u q s
  -- FUTUREWORK: Store the team id on elasticsearch as well. This would avoid
  --             the extra work done here and greatly simplify the query.
  return
    =<< if sameTeamSearchOnly
      then maybeFilterTeamUsers contacts
      else pure contacts
  where
    maybeFilterTeamUsers :: SearchResult Contact -> AppIO (SearchResult Contact)
    maybeFilterTeamUsers sresult = do
      selfTeam <- DB.lookupUserTeam u
      case selfTeam of
        Nothing -> pure sresult
        Just team -> filterTeamUsers sresult team
    -- Filter the result set with users from the given team only
    filterTeamUsers sresult team = do
      others <- DB.lookupUsersTeam $ fmap contactUserId (searchResults sresult)
      let sameTeamMembers = fmap fst $ filter ((== Just team) . snd) others
      let searchResultsFiltered = filter ((`elem` sameTeamMembers) . contactUserId) (searchResults sresult)
      return $
        sresult
          { searchReturned = length searchResultsFiltered,
            searchResults = searchResultsFiltered
          }

isSearchableH :: JSON ::: UserId -> Handler Response
isSearchableH (_ ::: u) = json <$> lift (isSearchable u)

isSearchable :: UserId -> AppIO SearchableStatus
isSearchable = checkIndex

setSearchableH :: UserId ::: JsonRequest SearchableStatus -> Handler Response
setSearchableH (u ::: r) = do
  s <- parseJsonBody r
  lift (setSearchable u s)
  return (setStatus status200 empty)

setSearchable :: UserId -> SearchableStatus -> AppIO ()
setSearchable u s = do
  DB.updateSearchableStatus u s
  Intra.onUserEvent u Nothing (searchableStatusUpdated u s)
  return ()
