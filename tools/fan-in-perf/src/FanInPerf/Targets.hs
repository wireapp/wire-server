module FanInPerf.Targets
  ( TargetKind (..),
    allKinds,
    kindName,
    TargetEntry (..),
    maxStreams,
    parseTargetSpec,
    renderTargetSpec,
    StreamPool (..),
    Entry (..),
    mkEntries,
    sampleDistinct,
    localDomain,
    targetAt,
    genTargets,
    forceTargets,
    targetKind,
    targetKey,
    mkPayload,
    mkPush,
  )
where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Domain
import Data.Id
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Data.UUID.Types qualified as UUID
import Data.Vector qualified as V
import Imports
import System.Random
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.Push.V2 (Route (RouteAny))
import Wire.FanInNotificationsStore

-- | One kind per 'Wire.FanInNotificationsStore.Target' constructor.
data TargetKind = KindUser | KindClients | KindTeam | KindEpoch | KindConnections
  deriving (Eq, Ord, Show, Enum, Bounded)

allKinds :: [TargetKind]
allKinds = [minBound .. maxBound]

kindName :: TargetKind -> Text
kindName = \case
  KindUser -> "user"
  KindClients -> "clients"
  KindTeam -> "team"
  KindEpoch -> "epoch"
  KindConnections -> "connections"

-- | @KIND:STREAMS[xK]@: @streams@ distinct stream keys, @perPush@ targets per push.
data TargetEntry = TargetEntry
  { kind :: TargetKind,
    streams :: Int,
    perPush :: Int
  }
  deriving (Eq, Show)

-- | Stream keys are kept in memory, so their number is bounded.
maxStreams :: Int
maxStreams = 10_000_000

parseTargetSpec :: Text -> Either String (NonEmpty TargetEntry)
parseTargetSpec spec = do
  entries <- traverse parseEntry (T.splitOn "," spec)
  let kinds = map (.kind) entries
  when (Set.size (Set.fromList kinds) /= length kinds) $
    Left "duplicate target kind"
  maybe (Left "empty target spec") Right (nonEmpty entries)

parseEntry :: Text -> Either String TargetEntry
parseEntry entry = case T.splitOn ":" entry of
  [k, counts] -> do
    kind <- parseKind k
    (streams, perPush) <- case T.splitOn "x" counts of
      [s] -> (,1) <$> parseCount s
      [s, p] -> (,) <$> parseCount s <*> parseCount p
      _ -> malformed
    when (perPush > streams) $
      Left ("targets per push exceed streams: " <> T.unpack entry)
    pure TargetEntry {..}
  _ -> malformed
  where
    malformed :: Either String a
    malformed = Left ("malformed target entry (expected KIND:STREAMS[xK]): " <> T.unpack entry)

parseKind :: Text -> Either String TargetKind
parseKind t =
  maybe (Left ("unknown target kind: " <> T.unpack t)) Right $
    find ((== t) . kindName) allKinds

-- | Parsed as 'Integer' first so huge inputs cannot overflow 'Int'.
parseCount :: Text -> Either String Int
parseCount t = case T.decimal @Integer t of
  Right (n, rest)
    | T.null rest && n > 0 && n <= toInteger maxStreams -> Right (fromInteger n)
  _ -> Left ("expected a number between 1 and " <> show maxStreams <> ", got: " <> T.unpack t)

renderTargetSpec :: NonEmpty TargetEntry -> Text
renderTargetSpec =
  T.intercalate ","
    . map (\e -> kindName e.kind <> ":" <> T.pack (show e.streams) <> "x" <> T.pack (show e.perPush))
    . NE.toList

-- | Pre-generated stream keys of one '--targets' entry.
data StreamPool
  = UserPool (V.Vector UserId)
  | ClientsPool (V.Vector (UserId, NonEmpty ClientId))
  | TeamPool (V.Vector TeamId)
  | EpochPool (V.Vector (GroupId, Epoch))
  | ConnectionsPool (V.Vector UserId)

data Entry = Entry
  { spec :: TargetEntry,
    pool :: StreamPool
  }

mkEntries :: (RandomGen g) => Int -> NonEmpty TargetEntry -> g -> (V.Vector Entry, g)
mkEntries clientsPerUser specs g0 =
  let step (acc, g) s =
        let (p, g') = mkPool clientsPerUser s g
         in (Entry s p : acc, g')
      (entries, g1) = foldl' step ([], g0) (NE.toList specs)
   in (V.fromList (reverse entries), g1)

mkPool :: (RandomGen g) => Int -> TargetEntry -> g -> (StreamPool, g)
mkPool clientsPerUser s g0 =
  let (g1, g2) = split g0
   in ( case s.kind of
          KindUser -> UserPool (V.unfoldrExactN s.streams genId g1)
          KindClients -> ClientsPool (V.unfoldrExactN s.streams genUserClients g1)
          KindTeam -> TeamPool (V.unfoldrExactN s.streams genId g1)
          KindEpoch -> EpochPool (V.unfoldrExactN s.streams genGroupEpoch g1)
          KindConnections -> ConnectionsPool (V.unfoldrExactN s.streams genId g1),
        g2
      )
  where
    -- Client ids 1..C per user: distinct, so one push never inserts the same
    -- (user, client, notification) row twice.
    clientIds = ClientId 1 :| map ClientId [2 .. fromIntegral clientsPerUser]
    genUserClients g = let (u, g') = genId g in ((u, clientIds), g')

genId :: (RandomGen g) => g -> (Id a, g)
genId g0 =
  let (w1, g1) = uniform g0
      (w2, g2) = uniform g1
   in (Id (UUID.fromWords64 w1 w2), g2)

-- | MLS group ids are arbitrary bytes; 32 random bytes like real ones.
genGroupEpoch :: (RandomGen g) => g -> ((GroupId, Epoch), g)
genGroupEpoch g0 =
  let (bytes, g1) = genByteString 32 g0
      (epoch, g2) = uniformR (0, 1000) g1
   in ((GroupId bytes, Epoch epoch), g2)

-- | @k@ distinct indices from @[0, n)@ using Floyd's algorithm: exactly @k@
-- random draws regardless of how close @k@ is to @n@. Requires @1 <= k <= n@.
sampleDistinct :: (RandomGen g) => Int -> Int -> g -> ([Int], g)
sampleDistinct n k = go (n - k) IntSet.empty []
  where
    go j seen acc g
      | j >= n = (acc, g)
      | otherwise =
          let (t, g') = uniformR (0, j) g
              pick = if IntSet.member t seen then j else t
           in go (j + 1) (IntSet.insert pick seen) (pick : acc) g'

-- | Domain of every generated qualified target; the store only needs it to be consistent.
localDomain :: Domain
localDomain = Domain "example.com"

targetAt :: StreamPool -> Int -> Target
targetAt pool i = case pool of
  UserPool v -> TargetUser (v V.! i)
  ClientsPool v -> TargetUserClients (v V.! i)
  TeamPool v -> TargetTeam (v V.! i)
  EpochPool v -> TargetEpoch (v V.! i)
  ConnectionsPool v -> TargetConnections (Qualified (v V.! i) localDomain)

-- REVIEW: This looks overly complicated!

-- | Picks an entry uniformly, then 'perPush' distinct stream keys of it. All
-- targets of one push therefore share one constructor. Targets are sorted by
-- pool index (pools are fixed, so this is one global order, and client ids per
-- user are ascending): the store upserts rows in target order and does not
-- sort itself, so unsorted overlapping pushes would deadlock (40P01).
genTargets :: (RandomGen g) => V.Vector Entry -> g -> ((TargetKind, NonEmpty Target), g)
genTargets entries g0 =
  let (ei, g1) = uniformR (0, V.length entries - 1) g0
      e = entries V.! ei
      (idxs, g2) = sampleDistinct e.spec.streams e.spec.perPush g1
      -- perPush >= 1, so idxs is never empty; the fallback only satisfies the type
      targets = targetAt e.pool <$> fromMaybe (0 :| []) (nonEmpty (sort idxs))
   in ((e.spec.kind, targets), g2)

-- REVIEW: Do we need this? Can the data type just be strict?

-- | Cheap full evaluation of the (otherwise lazily built) targets, so
-- generation cost stays out of the timed store call.
forceTargets :: NonEmpty Target -> ()
forceTargets = foldr (seq . forceTarget) ()
  where
    forceTarget = \case
      TargetUser u -> u `seq` ()
      TargetUserClients (u, cs) -> u `seq` foldr seq () cs
      TargetTeam t -> t `seq` ()
      TargetEpoch (g, e) -> g `seq` e `seq` ()
      TargetConnections q -> qUnqualified q `seq` qDomain q `seq` ()

targetKind :: Target -> TargetKind
targetKind = \case
  TargetUser _ -> KindUser
  TargetUserClients _ -> KindClients
  TargetTeam _ -> KindTeam
  TargetEpoch _ -> KindEpoch
  TargetConnections _ -> KindConnections

-- | Stream key as text, for tests and diagnostics.
targetKey :: Target -> Text
targetKey = \case
  TargetUser uid -> idToText uid
  TargetUserClients (uid, cids) -> idToText uid <> ":" <> T.intercalate "," (map clientToText (NE.toList cids))
  TargetTeam tid -> idToText tid
  TargetEpoch (gid, epoch) -> T.pack (show gid.unGroupId) <> "/" <> T.pack (show epoch.epochNumber)
  TargetConnections q -> idToText (qUnqualified q) <> "@" <> domainText (qDomain q)

-- | JSON payload of roughly @n@ bytes.
mkPayload :: Int -> A.Object
mkPayload n =
  KM.fromList
    [ ("type", A.String "fanin-perf"),
      ("pad", A.String (T.replicate n "x"))
    ]

mkPush :: A.Object -> NonEmpty Target -> FanInPush
mkPush payload targets =
  FanInPush
    { conn = Nothing,
      transient = False,
      route = RouteAny,
      nativePriority = Nothing,
      origin = Nothing,
      targets = NE.toList targets,
      json = payload,
      apsData = Nothing,
      isCellsEvent = False
    }
