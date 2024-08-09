module Wire.UserSearchSubsystem.Interpreter where

import Control.Error (MaybeT (..))
import Data.Domain
import Data.Id
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Team.Feature
import Wire.API.User.Search
import Wire.GalleyAPIAccess
import Wire.UserSearch.Types
import Wire.UserSearchSubsystem
import Wire.UserStore
import Wire.UserStore.IndexUser
import Wire.UserStore.IndexUser (indexUserRowToDoc)

interpretUserSearchSubsystem :: InterpreterFor UserSearchSubsystem r
interpretUserSearchSubsystem = interpret \case
  UpsertUser uid -> upsertUserImpl uid
  SearchUser luid query mDomain mMaxResults -> searchUserImpl luid query mDomain mMaxResults
  BrowseTeam uid browseTeamFilters mMaxResults mPagingState -> do
    browseTeamImpl uid browseTeamFilters mMaxResults mPagingState

upsertUserImpl :: (Member UserStore r, Member (Error String) r, Member GalleyAPIAccess r) => UserId -> Sem r ()
upsertUserImpl uid =
  note "aaaaa" =<< runMaybeT do
    indexUser <- MaybeT $ getIndexUser uid
    vis <-
      lift $
        maybe
          (pure defaultSearchVisibilityInbound)
          (fmap (searchVisibilityInboundFromFeatureStatus . wsStatus) . getFeatureConfigForTeam @_ @SearchVisibilityInboundConfig)
          indexUser.teamId
    let userDoc = indexUserRowToDoc vis indexUser
    IndexedUserStore.upsert docId userDoc

-- (IndexUpdateUser updateType iu) :: IndexUpdateUser IndexDocUpdateType IndexUser
-- ...  GetIndexUserRow

-- Prom.incCounter indexUpdateCounter
-- info $
--   field "user" (Bytes.toByteString (view iuUserId iu))
--     . msg (val "Indexing user")
-- idx <- asks idxName
-- withDefaultESUrl $ indexDoc idx
-- withAdditionalESUrl $ traverse_ indexDoc =<< asks idxAdditionalName
-- where
--   indexDoc :: (MonadIndexIO m, MonadThrow m) => ES.IndexName -> ES.BH m ()
--   indexDoc idx = do
--     r <- ES.indexDocument idx mappingName versioning (indexToDoc iu) docId
--     unless (ES.isSuccess r || ES.isVersionConflict r) $ do
--       liftIO $ Prom.incCounter indexUpdateErrorCounter
--       ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id
--     liftIO $ Prom.incCounter indexUpdateSuccessCounter

--   versioning :: _
--   versioning =
--     ES.defaultIndexDocumentSettings
--       { ES.idsVersionControl = indexUpdateToVersionControl updateType (ES.ExternalDocVersion (docVersion (_iuVersion iu)))
--       }

--   docId = ES.DocId (view (iuUserId . re _TextId) iu)

searchUserImpl :: Local UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> Sem r [Contact]
searchUserImpl = undefined

browseTeamImpl :: UserId -> BrowseTeamFilters -> Maybe (Range 1 500 Int32) -> Maybe PagingState -> Sem r [TeamContact]
browseTeamImpl = undefined

{-
updateIndex (IndexDeleteUser u) = liftIndexIO $ do
  Prom.incCounter indexDeleteCounter
  info $
    field "user" (Bytes.toByteString u)
      . msg (val "(Soft) deleting user from index")
  idx <- asks idxName
  r <- ES.getDocument idx mappingName (ES.DocId (review _TextId u))
  case statusCode (responseStatus r) of
    200 -> case preview (key "_version" . _Integer) (responseBody r) of
      Nothing -> throwM $ ES.EsProtocolException "'version' not found" (responseBody r)
      Just v -> updateIndex . IndexUpdateUser IndexUpdateIfNewerVersion . mkIndexUser u =<< mkIndexVersion (v + 1)
    404 -> pure ()
    _ -> ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id
-}
