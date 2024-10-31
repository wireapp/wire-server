module Galley.API.Teams.Export (getTeamMembersCSV) where

import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Lens (view, (^.))
import Control.Monad.Codensity
import Data.ByteString (toStrict)
import Data.ByteString.Builder
import Data.Csv
import Data.Handle
import Data.IORef (atomicModifyIORef, newIORef)
import Data.Id
import Data.Map qualified as Map
import Data.Qualified (Local, tUnqualified)
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.SparAccess qualified as Spar
import Galley.Effects.TeamMemberStore (listTeamMembers)
import Galley.Effects.TeamStore
import Imports hiding (atomicModifyIORef, newEmptyMVar, newIORef, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Polysemy
import Polysemy.Async
import Polysemy.Resource
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Routes.LowLevelStream (LowLevelStreamingBody)
import Wire.API.Team.Export
import Wire.API.Team.Member
import Wire.API.User (ScimUserInfo (suiCreatedOn), User (..))
import Wire.Sem.Concurrency
import Wire.Sem.Concurrency.IO
import Wire.Sem.Paging qualified as E
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | Cache of inviter handles.
--
-- This is used to make sure that inviters are only looked up once in brig,
-- even if they appear as inviters of several users in the team.
type InviterCache = IORef (Map UserId (MVar (Maybe Handle)))

lookupInviter ::
  (Member Resource r, Member BrigAccess r, Member (Final IO) r) =>
  InviterCache ->
  UserId ->
  Sem r (Maybe Handle)
lookupInviter cache uid = flip onException ensureCache $ do
  empty <- embedFinal newEmptyMVar
  (cached, var) <-
    embedFinal $ atomicModifyIORef cache $ \m -> case Map.lookup uid m of
      Nothing -> (Map.insert uid empty m, (False, empty))
      Just v -> (m, (True, v))
  -- the cache did not contain this user, so write it in the corresponding MVar
  unless cached $ do
    u <- listToMaybe <$> getUsers [uid]
    embedFinal $ putMVar var (u >>= userHandle)
  -- at this point, we know that the MVar contains a value or some other thread
  -- is about to write one, so it is safe to just read from the MVar with a
  -- blocking call
  embedFinal $ readMVar var
  where
    -- this is run in case of errors to guarantee that other threads will never
    -- deadlock while reading the cache
    ensureCache = embedFinal $ do
      m <- readIORef cache
      for_ (Map.lookup uid m) $ \var ->
        tryPutMVar var Nothing

getUserRecord ::
  ( Member BrigAccess r,
    Member Spar.SparAccess r,
    Member (ErrorS TeamMemberNotFound) r,
    Member (Final IO) r,
    Member Resource r
  ) =>
  InviterCache ->
  TeamMember ->
  Sem r TeamExportUser
getUserRecord cache member = do
  let uid = member ^. userId
  export <- getUserExportData uid >>= noteS @TeamMemberNotFound
  mCreatedOn <- do
    let mFromInvitation = snd <$> member ^. invitation
    case mFromInvitation of
      Just ts -> pure $ Just ts
      Nothing -> suiCreatedOn <$> Spar.lookupScimUserInfo uid
  -- look up inviter handle from the cache
  let mInviterId = fst <$> member ^. invitation
  invitedBy <- join <$> traverse (lookupInviter cache) mInviterId
  pure
    export
      { tExportInvitedBy = invitedBy,
        tExportRole = permissionsRole . view permissions $ member,
        tExportCreatedOn = mCreatedOn
      }

-- | Export team info as a CSV, and stream it to the client.
--
-- We paginate through the team member list, then spawn a thread for each user
-- (out of a thread pool) in order to fetch information for that user from brig
-- and spar. Inviter IDs are resolved to handles via a brig request, then
-- stored in a cache so that they can be reused by subsequent requests.
getTeamMembersCSV ::
  forall r.
  ( Member BrigAccess r,
    Member (ErrorS 'AccessDenied) r,
    Member (TeamMemberStore InternalPaging) r,
    Member TeamStore r,
    Member (Final IO) r,
    Member SparAccess r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r LowLevelStreamingBody
getTeamMembersCSV lusr tid = do
  getTeamMember tid (tUnqualified lusr) >>= \case
    Nothing -> throwS @'AccessDenied
    Just member -> unless (member `hasPermission` DownloadTeamMembersCsv) $ throwS @'AccessDenied

  chan <- embedFinal newChan
  cache <- embedFinal $ newIORef mempty

  let encodeRow r = encodeDefaultOrderedByNameWith customEncodeOptions [r]
  let produceTeamExportUsers = do
        embedFinal $ writeChan chan (Just headerLine)
        E.withChunks (\mps -> listTeamMembers @InternalPaging tid mps maxBound) $
          \members -> unsafePooledForConcurrentlyN_ 8 members $ \member -> do
            mRecord <-
              runErrorS @TeamMemberNotFound $
                getUserRecord cache member
            let mRow = encodeRow <$> mRecord
            when (isJust mRow) $
              embedFinal $
                writeChan chan mRow

  -- In case an exception is thrown inside the producer thread, the response
  -- will not contain a correct error message, but rather be an http error such
  -- as 'InvalidChunkHeaders'. The exception however still reaches the
  -- middleware and is being tracked in logging and metrics.
  let producerThread =
        produceTeamExportUsers
          `finally` embedFinal (writeChan chan Nothing)

  asyncToIOFinal . resourceToIOFinal . unsafelyPerformConcurrency @_ @Unsafe $ do
    -- Here we should really capture the Wai continuation and run the finaliser
    -- after that. Unfortunately, this is not really possible with Servant,
    -- because the continuation is not exposed by the Handler monad. The best
    -- we can do is return a Codensity value with the correct finaliser, but
    -- that still leaves a short window between when the resource is acquired
    -- and when the finaliser is installed where the resource might be leaked.
    -- I don't have a good solution for that.
    bracketOnError
      (async producerThread)
      cancel
      $ \producer -> do
        pure $ do
          void $ Codensity $ \k -> do
            r <- k ()
            Async.cancel producer
            pure r
          pure $ \write flush -> do
            let go = do
                  readChan chan >>= \case
                    Nothing -> write "" >> flush
                    Just line -> write (byteString (toStrict line)) >> flush >> go
            go

headerLine :: LByteString
headerLine = encodeDefaultOrderedByNameWith (customEncodeOptions {encIncludeHeader = True}) ([] :: [TeamExportUser])

customEncodeOptions :: EncodeOptions
customEncodeOptions =
  EncodeOptions
    { encDelimiter = fromIntegral (ord ','),
      encUseCrLf = True, -- to be compatible with Mac and Windows
      encIncludeHeader = False, -- (so we can flush when the header is on the wire)
      encQuoting = QuoteAll
    }
