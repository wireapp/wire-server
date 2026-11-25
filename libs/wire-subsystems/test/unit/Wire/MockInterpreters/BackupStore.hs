-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedRecordDot #-}

module Wire.MockInterpreters.BackupStore where

import Data.Id
import Data.Json.Util (fromUTCTimeMillis, toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time.Clock (UTCTime, addUTCTime)
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Backup
import Wire.BackupStore
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

-- | A row of @backup_orphans@.
data MockOrphan = MockOrphan
  { userId :: UserId,
    clientId :: ClientId,
    -- | 'Nothing' for upload reservations.
    fileCreatedAt :: Maybe UTCTime,
    orphanedAt :: UTCTime,
    deletionLastTried :: Maybe UTCTime
  }
  deriving (Eq, Show)

data BackupState = BackupState
  { -- | Epoch creation time and full of each epoch.
    epochs :: Map (UserId, ClientId, BackupEpoch) (UTCTime, BackupFile),
    orphans :: Map BackupFileId MockOrphan,
    -- | Incrementals with the full they belong to.
    incrementals :: Map BackupFileId (BackupFileId, BackupFile)
  }
  deriving (Eq, Show)

emptyBackupState :: BackupState
emptyBackupState = BackupState {epochs = mempty, orphans = mempty, incrementals = mempty}

-- | Mirrors the transaction of "Wire.BackupStore.Postgres".
inMemoryBackupStore ::
  (Member (State BackupState) r, Member Now r) =>
  InterpreterFor BackupStore r
inMemoryBackupStore = interpret $ \case
  ReserveUpload uid cid _ fid -> do
    now <- Now.get
    modify $ \s -> s {orphans = Map.insert fid (newOrphan uid cid now Nothing) s.orphans}
  CommitFull uid cid epoch fid size -> do
    s <- get
    now <- Now.get
    let maxEpoch = maxEpochOf s uid cid
        file = BackupFile {id = fid, size = size, createdAt = toUTCTimeMillis now}
    case () of
      _
        | not (epochAccepted maxEpoch epoch) -> pure (Left CommitEpochInvalid)
        | not (isReservation fid s) -> pure (Left CommitReservationGone)
        | otherwise -> do
            let key = (uid, cid, epoch)
                withoutReservation = Map.delete fid s.orphans
                replacedFull = (.id) . snd <$> Map.lookup key s.epochs
                replaced = case Map.lookup key s.epochs of
                  Nothing -> mempty
                  Just (_, old) ->
                    Map.singleton old.id (newOrphan uid cid now (Just (fromUTCTimeMillis old.createdAt)))
                inserted = Map.insert key (now, file) s.epochs
                isStale (u, c, e) = u == uid && c == cid && epoch >= BackupEpoch 3 && e <= staleEpochs epoch
                (stale, kept) = Map.partitionWithKey (\k _ -> isStale k) inserted
                staleOrphans =
                  Map.fromList
                    [(f.id, newOrphan uid cid now (Just (fromUTCTimeMillis f.createdAt))) | (_, f) <- Map.elems stale]
                -- the incrementals of replaced and stale fulls are orphaned as well
                orphanedFulls = Set.fromList (toList replacedFull <> [f.id | (_, f) <- Map.elems stale])
                (goneIncs, keptIncs) = Map.partition ((`Set.member` orphanedFulls) . fst) s.incrementals
                incOrphans =
                  Map.map (newOrphan uid cid now . Just . fromUTCTimeMillis . (.createdAt) . snd) goneIncs
                newOrphans = replaced <> staleOrphans <> incOrphans
            put
              BackupState
                { epochs = kept,
                  orphans = withoutReservation <> newOrphans,
                  incrementals = keptIncs
                }
            pure (Right (file, Map.keys newOrphans))
  CommitIncremental maxFiles uid cid epoch full fid size -> do
    s <- get
    now <- Now.get
    let file = BackupFile {id = fid, size = size, createdAt = toUTCTimeMillis now}
        current = (.id) . snd <$> Map.lookup (uid, cid, epoch) s.epochs
        count = 1 + fromIntegral (Map.size (Map.filter ((== full) . fst) s.incrementals))
    case () of
      _
        | current /= Just full -> pure (Left CommitFullMissing)
        | count >= maxFiles -> pure (Left CommitEpochFull)
        | not (isReservation fid s) -> pure (Left CommitIncrementalReservationGone)
        | otherwise -> do
            put
              s
                { orphans = Map.delete fid s.orphans,
                  incrementals = Map.insert fid (full, file) s.incrementals
                }
            pure (Right file)
  LookupMaxEpoch uid cid -> gets (\s -> maxEpochOf s uid cid)
  LookupEpochFiles uid cid epoch ->
    gets $ \s ->
      (\(_, f) -> BackupFiles {full = f, incrementals = incrementalsOf s f.id})
        <$> Map.lookup (uid, cid, epoch) s.epochs
  LookupFull uid cid epoch fid ->
    gets $ \s -> fmap ((.id) . snd) (Map.lookup (uid, cid, epoch) s.epochs) == Just fid
  LookupIncremental uid cid epoch full fid ->
    gets $ \s ->
      fmap ((.id) . snd) (Map.lookup (uid, cid, epoch) s.epochs) == Just full
        && fmap fst (Map.lookup fid s.incrementals) == Just full
  CountEpochFiles uid cid epoch full ->
    gets $ \s -> case Map.lookup (uid, cid, epoch) s.epochs of
      Just (_, f) | f.id == full -> Just (1 + fromIntegral (length (incrementalsOf s f.id)))
      _ -> Nothing
  ListBackupClients uid ->
    gets $ \s ->
      Set.toList . Set.fromList $
        [c | (u, c, _) <- Map.keys s.epochs, u == uid]
          <> [o.clientId | o <- Map.elems s.orphans, o.userId == uid, isNothing o.fileCreatedAt]
  OrphanClient uid cid -> do
    s <- get
    now <- Now.get
    let ofClient = Map.filterWithKey (\(u, c, _) _ -> u == uid && c == cid) s.epochs
        fulls = Set.fromList [f.id | (_, f) <- Map.elems ofClient]
        (movedIncs, keptIncs) = Map.partition ((`Set.member` fulls) . fst) s.incrementals
        orphanOf = newOrphan uid cid now . Just . fromUTCTimeMillis . (.createdAt)
        newOrphans =
          Map.map (orphanOf . snd) movedIncs
            <> Map.fromList [(f.id, orphanOf f) | (_, f) <- Map.elems ofClient]
        blocked o
          | o.userId == uid && o.clientId == cid && isNothing o.fileCreatedAt =
              o {deletionLastTried = o.deletionLastTried <|> Just now}
          | otherwise = o
    put
      s
        { epochs = Map.difference s.epochs ofClient,
          incrementals = keptIncs,
          orphans = Map.map blocked s.orphans <> newOrphans
        }
    pure (Map.keys newOrphans)
  SelectCleanupBatch grace limit -> do
    now <- Now.get
    s <- get
    let eligible o = isJust o.fileCreatedAt || o.orphanedAt < addUTCTime (negate grace) now
        picked =
          take (fromIntegral limit)
            . map fst
            . sortOn (\(_, o) -> (isJust o.deletionLastTried, o.deletionLastTried, o.orphanedAt))
            . filter (eligible . snd)
            $ Map.toList s.orphans
    put s {orphans = foldr (Map.adjust (\o -> o {deletionLastTried = Just now})) s.orphans picked}
    pure picked
  DeleteOrphanRows ids ->
    modify $ \s -> s {orphans = foldr Map.delete s.orphans ids}
  MarkDeletionTried ids -> do
    now <- Now.get
    modify $ \s ->
      s {orphans = foldr (Map.adjust (\o -> o {deletionLastTried = Just now})) s.orphans ids}
  ListClientBackups uid mcid ->
    gets $ \s ->
      [ ClientBackups {client = cid, epochs = es}
      | (cid, es) <- Map.toAscList (Map.fromListWith (flip (<>)) (summaries s))
      ]
    where
      summaries s =
        [ (cid, [summaryOf s epoch full])
        | ((u, cid, epoch), (_, full)) <- Map.toAscList s.epochs,
          u == uid,
          maybe True (== cid) mcid
        ]

newOrphan :: UserId -> ClientId -> UTCTime -> Maybe UTCTime -> MockOrphan
newOrphan uid cid now created =
  MockOrphan {userId = uid, clientId = cid, fileCreatedAt = created, orphanedAt = now, deletionLastTried = Nothing}

-- | Can the commit claim the file: an upload reservation not marked by 'OrphanClient'?
isReservation :: BackupFileId -> BackupState -> Bool
isReservation fid s =
  fmap (\o -> (o.fileCreatedAt, o.deletionLastTried)) (Map.lookup fid s.orphans)
    == Just (Nothing, Nothing)

-- | Mirrors the aggregate query of "Wire.BackupStore.Postgres".
summaryOf :: BackupState -> BackupEpoch -> BackupFile -> EpochSummary
summaryOf s epoch full =
  let incs = incrementalsOf s full.id
   in EpochSummary
        { epoch = epoch,
          createdAt = maximum (full.createdAt : map (.createdAt) incs),
          fileCount = 1 + fromIntegral (length incs),
          size = full.size + sum (map (.size) incs)
        }

-- | The incrementals of a full, oldest first.
incrementalsOf :: BackupState -> BackupFileId -> [BackupFile]
incrementalsOf s full =
  sortOn (\f -> (f.createdAt, f.id)) [f | (full', f) <- Map.elems s.incrementals, full' == full]

maxEpochOf :: BackupState -> UserId -> ClientId -> Maybe BackupEpoch
maxEpochOf s uid cid =
  fmap fst . Map.lookupMax . Map.fromList $
    [(e, ()) | (u, c, e) <- Map.keys s.epochs, u == uid, c == cid]
