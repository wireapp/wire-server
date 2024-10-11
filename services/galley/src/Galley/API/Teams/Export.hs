module Galley.API.Teams.Export (getTeamMembersCSV) where

import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Error (MaybeT (MaybeT, runMaybeT))
import Control.Lens (view, (^.))
import Control.Monad.Codensity
import Data.ByteString (toStrict)
import Data.ByteString.Builder
import Data.Csv
import Data.Id
import Data.Qualified (Local, tUnqualified)
import Debug.Trace
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.SparAccess qualified as Spar
import Galley.Effects.TeamMemberStore (listTeamMembers)
import Galley.Effects.TeamStore
import Imports hiding (newEmptyMVar, putMVar, takeMVar, threadDelay)
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

getUserRecord ::
  ( Member BrigAccess r,
    Member Spar.SparAccess r
  ) =>
  TeamMember ->
  Sem r (Maybe TeamExportUser)
getUserRecord member = runMaybeT do
  let uid = member ^. userId
  export <- MaybeT $ getUserExportData uid
  mCreatedOn <- do
    let mFromInvitation = snd <$> member ^. invitation
    case mFromInvitation of
      Just ts -> pure $ Just ts
      Nothing -> do
        -- TODO: make this a single user query
        suis <- lift $ Spar.lookupScimUserInfos [uid]
        pure $ listToMaybe suis >>= suiCreatedOn
  -- TODO: optimize!
  let mInviterId = fst <$> member ^. invitation
  users <- lift $ getUsers (maybeToList mInviterId)
  let invitedBy = listToMaybe users >>= userHandle
  pure
    export
      { tExportInvitedBy = invitedBy,
        tExportRole = permissionsRole . view permissions $ member,
        tExportCreatedOn = mCreatedOn
      }

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

  let encodeRow r = encodeDefaultOrderedByNameWith customEncodeOptions [r]
  let produceTeamExportUsers = do
        embedFinal $ writeChan chan (Just headerLine)
        E.withChunks (\mps -> listTeamMembers @InternalPaging tid mps maxBound) $
          \members -> unsafePooledForConcurrentlyN_ 8 members $ \member -> do
            mRecord <- getUserRecord member
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
                  traceM "write chunk"
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
