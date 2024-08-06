module Wire.UserSearchSubsystem.Interpreter where

import Cassandra.Exec (paginateWithStateC)
import Conduit (ConduitT, runConduit, (.|))
import Data.Conduit.Combinators qualified as Conduit
import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Database.Bloodhound.Types qualified as ES
import Imports
import Polysemy
import Polysemy.TinyLog
import System.Logger.Message qualified as Log
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti (TeamStatus (..))
import Wire.API.Team.Feature
import Wire.API.User.Search
import Wire.GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserStore)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe), unsafePooledForConcurrentlyN)
import Wire.UserSearch.Types
import Wire.UserSearchSubsystem
import Wire.UserStore
import Wire.UserStore.IndexUser

interpretUserSearchSubsystem ::
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  InterpreterFor UserSearchSubsystem r
interpretUserSearchSubsystem = interpret \case
  SyncUser uid -> syncUserImpl uid
  UpdateTeamSearchVisibilityInbound status -> updateTeamSearchVisibilityInboundImpl status
  SearchUser luid query mDomain mMaxResults -> searchUserImpl luid query mDomain mMaxResults
  BrowseTeam uid browseTeamFilters mMaxResults mPagingState -> do
    browseTeamImpl uid browseTeamFilters mMaxResults mPagingState

interpretUserSearchSubsystemBulk ::
  ( Member TinyLog r,
    Member UserStore r,
    Member (Concurrency Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  InterpreterFor UserSearchSubsystemBulk r
interpretUserSearchSubsystemBulk = interpret \case
  SyncAllUsers -> syncAllUsersImpl (ES.ExternalGT)
  ForceSyncAllUsers -> syncAllUsersImpl (ES.ExternalGTE)

syncUserImpl ::
  forall r.
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  UserId ->
  Sem r ()
syncUserImpl uid =
  getIndexUser uid
    >>= maybe delete upsert
  where
    delete :: Sem r ()
    delete = do
      IndexedUserStore.upsert (docId uid) (emptyUserDoc uid) ES.NoVersionControl

    upsert :: IndexUser -> Sem r ()
    upsert indexUser = do
      vis <-
        maybe
          (pure defaultSearchVisibilityInbound)
          teamSearchVisibilityInbound
          indexUser.teamId
      let userDoc = indexUserRowToDoc vis indexUser
          version = ES.ExternalGT . ES.ExternalDocVersion . docVersion $ indexUserRowToVersion indexUser
      IndexedUserStore.upsert (docId uid) userDoc version

syncAllUsersImpl ::
  forall r.
  ( Member UserStore r,
    Member TinyLog r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  (ES.ExternalDocVersion -> ES.VersionControl) ->
  Sem r ()
syncAllUsersImpl mkVersion =
  runConduit $
    paginateWithStateC getIndexUsersPaginated
      .| logPage
      .| mkUserDocs
      .| Conduit.mapM_ IndexedUserStore.bulkUpsert
  where
    logPage :: ConduitT [IndexUser] [IndexUser] (Sem r) ()
    logPage = Conduit.iterM $ \page -> do
      info $
        Log.field "size" (length page)
          . Log.msg (Log.val "Reindex: processing C* page")

    mkUserDocs :: ConduitT [IndexUser] [(ES.DocId, UserDoc, ES.VersionControl)] (Sem r) ()
    mkUserDocs = Conduit.mapM $ \page -> do
      visMap <- fmap Map.fromList . unsafePooledForConcurrentlyN 16 (Set.fromList $ mapMaybe (.teamId) page) $ \t ->
        (t,) <$> teamSearchVisibilityInbound t
      let vis indexUser = fromMaybe defaultSearchVisibilityInbound $ flip Map.lookup visMap =<< indexUser.teamId
          mkUserDoc indexUser = indexUserRowToDoc (vis indexUser) indexUser
          mkDocVersion = mkVersion . ES.ExternalDocVersion . docVersion . indexUserRowToVersion
      pure $ map (\u -> (docId u.userId, mkUserDoc u, mkDocVersion u)) page

updateTeamSearchVisibilityInboundImpl :: (Member IndexedUserStore r) => TeamStatus SearchVisibilityInboundConfig -> Sem r ()
updateTeamSearchVisibilityInboundImpl teamStatus =
  IndexedUserStore.updateTeamSearchVisibilityInbound teamStatus.team $
    searchVisibilityInboundFromFeatureStatus teamStatus.status

searchUserImpl :: Local UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> Sem r [Contact]
searchUserImpl = undefined

browseTeamImpl :: UserId -> BrowseTeamFilters -> Maybe (Range 1 500 Int32) -> Maybe PagingState -> Sem r [TeamContact]
browseTeamImpl = undefined

docId :: UserId -> ES.DocId
docId uid = ES.DocId (idToText uid)

teamSearchVisibilityInbound :: (Member GalleyAPIAccess r) => TeamId -> Sem r SearchVisibilityInbound
teamSearchVisibilityInbound tid =
  searchVisibilityInboundFromFeatureStatus . (.status)
    <$> getFeatureConfigForTeam @_ @SearchVisibilityInboundConfig tid
