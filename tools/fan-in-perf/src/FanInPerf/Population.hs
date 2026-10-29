-- | The simulated readers: teams of users, each user with its clients. Derived
-- from the pools the writers use, so readers poll streams that get written.
-- A reader is one client of one user and fetches and acks as that
-- (user, client): 'readerStreams' derives its user and client streams from
-- the same pair.
module FanInPerf.Population
  ( SimTeam (..),
    SimUser (..),
    Reader (..),
    Key,
    streamKey,
    Population (..),
    mkPopulation,
    maxReaders,
    readers,
    readerCount,
    readerStreams,
    streamKind,
    expectedDeliveries,
    expectedFromCounts,
    subscribedRows,
    coverage,
  )
where

import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Misc (showT)
import Data.Set qualified as Set
import Data.Vector qualified as V
import FanInPerf.NotificationsStore
import FanInPerf.Targets
import Imports hiding (Reader)
import System.Random (StdGen)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group

data SimTeam = SimTeam
  { tid :: !TeamId,
    members :: !(NonEmpty SimUser)
  }

data SimUser = SimUser
  { uid :: !UserId,
    -- | [] without 'epoch' in --targets; shared by all clients of the user
    epochs :: ![(GroupId, Epoch)],
    -- | 1..C, one reader thread each
    clients :: !(NonEmpty ClientId)
  }

-- | What one reader thread needs.
data Reader = Reader
  { team :: !(Maybe TeamId),
    user :: !SimUser,
    cid :: !ClientId
  }

-- | Identifies one stream. 'Epoch' has no 'Ord', hence the plain number.
data Key
  = KUser !UserId
  | KClient !UserId !ClientId
  | KTeam !TeamId
  | KEpoch !GroupId !Word64
  deriving (Eq, Ord, Show)

data Population = Population
  { -- | 'team' in --targets: every user is in exactly one team ...
    teams :: [SimTeam],
    -- | ... otherwise all users are here.
    personalUsers :: [SimUser],
    -- | kinds present in --targets
    kinds :: Set TargetKind,
    -- | Number of readers of each stream (streams nobody reads are absent).
    subscribers :: Map Key Int
  }

readers :: Population -> [Reader]
readers p =
  [Reader (Just t.tid) u c | t <- p.teams, u <- toList t.members, c <- toList u.clients]
    <> [Reader Nothing u c | u <- p.personalUsers, c <- toList u.clients]

-- | Number of readers: users times clients per user.
readerCount :: Population -> Int
readerCount = length . readers

-- | The only place reader streams come from.
readerStreams :: Set TargetKind -> Reader -> [Stream]
readerStreams kinds r =
  [StreamUser r.user.uid | KindUser `Set.member` kinds]
    <> [StreamClient r.user.uid r.cid | KindClients `Set.member` kinds]
    <> [StreamTeam t | Just t <- [r.team]]
    <> [uncurry StreamEpoch e | e <- r.user.epochs]

-- | Upper bound of 'readerCount': every reader is a thread with its own state.
maxReaders :: Int
maxReaders = 100_000

-- | @mkPopulation onlineUsers epochsPerUser clientsPerUser entries gen@: the
-- online users each have @clientsPerUser@ clients (ids @1..C@), each one reader.
-- With a user/clients pool ('mkEntries') they are its first @onlineUsers@ users
-- (default: all, but at most 'maxReaders' readers), otherwise @onlineUsers@ (required) synthetic ones drawn from
-- @gen@. User @u@ is in team @u mod S_t@ and reads @E@ consecutive epoch streams
-- from @u * E@, shared by all its clients.
-- 'entries' is never empty (the CLI requires at least one target entry).
mkPopulation :: Maybe Int -> Int -> Int -> V.Vector Entry -> StdGen -> Either Text Population
mkPopulation onlineUsers epochs clientsPerUser entries gen = do
  when (any ((== KindConnections) . (.spec.kind)) es) $
    Left "connections targets are not supported by consume"
  let kinds = Set.fromList (map (.spec.kind) es)
      pool = poolUsers es
      maxOnline = maxReaders `div` clientsPerUser
  online <- case (pool, onlineUsers) of
    (Just (size, _), Nothing) -> Right (min size maxOnline)
    (Just (size, _), Just u)
      | u > size -> Left ("--online-users " <> showT u <> " exceeds the " <> showT size <> " users of the user/clients pool")
      | otherwise -> Right u
    (Nothing, Just u) -> Right u
    (Nothing, Nothing) -> Left "--online-users is required: --targets has no user or clients entry"
  -- compared by division: online * clientsPerUser may overflow
  when (online > maxOnline) $
    Left (showT online <> " online users (--online-users) x " <> showT clientsPerUser <> " (--clients-per-user) = " <> showT (toInteger online * toInteger clientsPerUser) <> " readers, at most " <> showT maxReaders <> " are supported")
  let uids = maybe (V.unfoldrExactN online genId gen) (\(_, firstUsers) -> firstUsers online) pool
  epochsOf <- userEpochs epochs es
  let clientIds = clientIdsUpTo clientsPerUser
      simUsers = [SimUser uid (epochsOf u) clientIds | (u, uid) <- zip [0 ..] (V.toList uids)]
      (teams, personalUsers) = case [v | Entry _ (TeamPool v) <- es] of
        v : _ -> (groupTeams v simUsers, [])
        [] -> ([], simUsers)
      p = Population {teams, personalUsers, kinds, subscribers = mempty}
  pure p {subscribers = Map.fromListWith (+) [(streamKey st, 1) | r <- readers p, st <- readerStreams kinds r]}
  where
    es = V.toList entries

-- | The user or clients pool (the CLI requires the same size for both, and
-- both hold the same uids, see 'mkEntries'): its size and its first @n@ uids
-- (only those are copied out of a clients pool).
poolUsers :: [Entry] -> Maybe (Int, Int -> V.Vector UserId)
poolUsers es =
  listToMaybe $
    [(V.length v, (`V.take` v)) | Entry _ (UserPool v) <- es]
      <> [(V.length v, \n -> V.map fst (V.take n v)) | Entry _ (ClientsPool v) <- es]

-- | @E@ consecutive epoch streams per user index; none without 'epoch'.
userEpochs :: Int -> [Entry] -> Either Text (Int -> [(GroupId, Epoch)])
userEpochs e es = case [v | Entry _ (EpochPool v) <- es] of
  [] -> Right (const [])
  v : _
    | e > V.length v -> Left ("epoch: --epochs-per-user " <> showT e <> " exceeds the " <> showT (V.length v) <> " epoch streams")
    | otherwise -> Right (\u -> [v V.! ((u * e + j) `mod` V.length v) | j <- [0 .. e - 1]])

-- | User @u@ joins team @u mod S_t@; teams without members are left out.
groupTeams :: V.Vector TeamId -> [SimUser] -> [SimTeam]
groupTeams v users =
  [ SimTeam (v V.! t) ms
  | (t, ms) <- Map.toList (Map.fromListWith (flip (<>)) [(u `mod` V.length v, su :| []) | (u, su) <- zip [0 ..] users])
  ]

streamKey :: Stream -> Key
streamKey = \case
  StreamUser u -> KUser u
  StreamClient u c -> KClient u c
  StreamTeam t -> KTeam t
  StreamEpoch g e -> KEpoch g e.epochNumber

streamKind :: Stream -> TargetKind
streamKind = \case
  StreamUser _ -> KindUser
  StreamClient _ _ -> KindClients
  StreamTeam _ -> KindTeam
  StreamEpoch _ _ -> KindEpoch

-- | The streams (as 'Key's) a push 'Target' writes to.
targetKeys :: Target -> [Key]
targetKeys = \case
  TargetUser u -> [KUser u]
  TargetUserClients (u, cs) -> [KClient u c | c <- NE.toList cs]
  TargetTeam t -> [KTeam t]
  TargetEpoch (g, e) -> [KEpoch g e.epochNumber]
  TargetConnections _ -> []

-- | Notifications that reach readers when this push succeeds.
expectedDeliveries :: Map Key Int -> FanInPush -> Int
expectedDeliveries subs push =
  sum [Map.findWithDefault 0 k subs | t <- push.targets, k <- targetKeys t]

-- | Notifications that reach readers when the streams hold the given number of
-- rows (stream, rows): rows times readers of the stream, so what 'expectedDeliveries'
-- adds up push by push.
expectedFromCounts :: Map Key Int -> [(Stream, Int)] -> Int
expectedFromCounts subs counts =
  sum [n * Map.findWithDefault 0 (streamKey s) subs | (s, n) <- counts]

-- | Rows in streams somebody reads.
subscribedRows :: Map Key Int -> [(Stream, Int)] -> Int
subscribedRows subs counts =
  sum [n | (s, n) <- counts, streamKey s `Map.member` subs]

-- | Per entry: share of its pool streams that have at least one reader.
coverage :: Map Key Int -> V.Vector Entry -> [(TargetKind, Double)]
coverage subs = map one . V.toList
  where
    one e =
      let covered = length [() | i <- [0 .. e.spec.streams - 1], any (`Map.member` subs) (targetKeys (targetAt e.pool i))]
       in (e.spec.kind, fromIntegral covered / fromIntegral e.spec.streams)
