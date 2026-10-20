module FanInPerf.Targets
  ( TargetKind (..),
    allKinds,
    kindName,
    TargetConfig (..),
    renderTargetSpec,
    StreamPool (..),
    Entry (..),
    mkEntries,
    clientIdsUpTo,
    genId,
    sampleDistinct,
    localDomain,
    targetAt,
    genTargets,
    genPush,
    targetKind,
    pushKind,
    targetKey,
    mkPayload,
  )
where

import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (first)
import Data.Domain
import Data.Id
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Misc (showT)
import Data.Qualified
import Data.Text qualified as T
import Data.Vector qualified as V
import Imports
import System.Random
import System.Random.Stateful (runStateGen, uniformByteStringM, uniformRM)
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
data TargetConfig = TargetConfig
  { kind :: TargetKind,
    streams :: Int,
    perPush :: Int
  }
  deriving (Eq, Show)

renderTargetSpec :: NonEmpty TargetConfig -> Text
renderTargetSpec =
  T.intercalate ","
    . map (\e -> kindName e.kind <> ":" <> showT e.streams <> "x" <> showT e.perPush)
    . NE.toList

-- | Pre-generated stream keys of one '--targets' entry.
data StreamPool
  = UserPool (V.Vector UserId)
  | ClientsPool (V.Vector (UserId, NonEmpty ClientId))
  | TeamPool (V.Vector TeamId)
  | EpochPool (V.Vector (GroupId, Epoch))
  | ConnectionsPool (V.Vector UserId)

data Entry = Entry
  { spec :: TargetConfig,
    pool :: StreamPool
  }

-- | The user and clients pools are prefixes of one uid vector, so a reader
-- client (user, client id) can read both the user's and its own client stream
-- (see "FanInPerf.Population"). Different tables, so no extra contention.
mkEntries :: (RandomGen g) => Int -> NonEmpty TargetConfig -> g -> (V.Vector Entry, g)
mkEntries clientsPerUser specs g0 =
  let (gUsers, g1) = split g0
      sharedUsers = maximum (0 : [s.streams | s <- NE.toList specs, s.kind `elem` [KindUser, KindClients]])
      users = V.unfoldrExactN sharedUsers genId gUsers
      step g s =
        let (p, g') = mkPool clientsPerUser users s g
         in (g', Entry s p)
      (g2, entries) = mapAccumL step g1 (NE.toList specs)
   in (V.fromList entries, g2)

mkPool :: (RandomGen g) => Int -> V.Vector UserId -> TargetConfig -> g -> (StreamPool, g)
mkPool clientsPerUser users s g0 =
  let (g1, g2) = split g0
   in ( case s.kind of
          KindUser -> UserPool (V.take s.streams users)
          KindClients -> ClientsPool (V.map (,clientIds) (V.take s.streams users))
          KindTeam -> TeamPool (V.unfoldrExactN s.streams genId g1)
          KindEpoch -> EpochPool (V.unfoldrExactN s.streams genGroupEpoch g1)
          KindConnections -> ConnectionsPool (V.unfoldrExactN s.streams genId g1),
        g2
      )
  where
    clientIds = clientIdsUpTo clientsPerUser

-- | Client ids 1..k (at least 1).
clientIdsUpTo :: Int -> NonEmpty ClientId
clientIdsUpTo k = ClientId 1 :| map ClientId [2 .. fromIntegral k]

genId :: (RandomGen g) => g -> (Id a, g)
genId = first Id . uniform

-- | MLS group ids are arbitrary bytes; 32 random bytes like real ones.
genGroupEpoch :: (RandomGen g) => g -> ((GroupId, Epoch), g)
genGroupEpoch g = runStateGen g $ \gen -> do
  bytes <- uniformByteStringM 32 gen
  epoch <- uniformRM (0, 1000) gen
  pure (GroupId bytes, Epoch epoch)

-- | @k@ distinct indices from @[0, n)@ using Floyd's algorithm: exactly @k@
-- random draws regardless of how close @k@ is to @n@. Requires @1 <= k <= n@.
-- Result is ascending.
sampleDistinct :: (RandomGen g) => Int -> Int -> g -> ([Int], g)
sampleDistinct n k = go (n - k) IntSet.empty
  where
    go j seen g
      | j >= n = (IntSet.toAscList seen, g)
      | otherwise =
          let (t, g') = uniformR (0, j) g
              pick = if IntSet.member t seen then j else t
           in go (j + 1) (IntSet.insert pick seen) g'

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

-- | Picks an entry uniformly, then 'perPush' distinct stream keys of it. All
-- targets of one push therefore share one constructor. Distinctness keeps the
-- fan-in exactly 'perPush' (the store dedupes and sorts on its own).
genTargets :: (RandomGen g) => V.Vector Entry -> g -> (NonEmpty Target, g)
genTargets entries g0 =
  let (ei, g1) = uniformR (0, V.length entries - 1) g0
      e = entries V.! ei
      (idxs, g2) = sampleDistinct e.spec.streams e.spec.perPush g1
      -- perPush >= 1, so idxs is never empty; the fallback only satisfies the type
      targets = targetAt e.pool <$> fromMaybe (0 :| []) (nonEmpty idxs)
   in (targets, g2)

-- | A random push with the given payload and 'genTargets' targets.
genPush :: (RandomGen g) => A.Object -> V.Vector Entry -> g -> (FanInPush, g)
genPush payload entries g =
  let (targets, g') = genTargets entries g
   in (mkPush payload targets, g')

targetKind :: Target -> TargetKind
targetKind = \case
  TargetUser _ -> KindUser
  TargetUserClients _ -> KindClients
  TargetTeam _ -> KindTeam
  TargetEpoch _ -> KindEpoch
  TargetConnections _ -> KindConnections

-- | Kind of a push; all its targets share one constructor.
pushKind :: FanInPush -> TargetKind
pushKind push = case push.targets of
  t : _ -> targetKind t
  -- 'genPush' never produces a push without targets
  [] -> minBound

-- | Stream key as text, for tests and diagnostics.
targetKey :: Target -> Text
targetKey = \case
  TargetUser uid -> idToText uid
  TargetUserClients (uid, cids) -> idToText uid <> ":" <> T.intercalate "," (map clientToText (NE.toList cids))
  TargetTeam tid -> idToText tid
  TargetEpoch (gid, epoch) -> showT gid.unGroupId <> "/" <> showT epoch.epochNumber
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
