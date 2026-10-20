-- | Which streams each simulated reader client subscribes to. Derived from the
-- same stream pools the writers use, so readers poll streams that get written.
module FanInPerf.Population
  ( Client (..),
    Key,
    Population (..),
    mkPopulation,
    streamKind,
    expectedDeliveries,
    coverage,
  )
where

import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import FanInPerf.Targets
import Imports
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.FanInNotificationsStore

newtype Client = Client {streams :: [Stream]}

-- | Identifies one stream. 'Epoch' has no 'Ord', hence the plain number.
data Key
  = KUser !UserId
  | KClient !UserId !ClientId
  | KTeam !TeamId
  | KEpoch !GroupId !Word64
  deriving (Eq, Ord, Show)

data Population = Population
  { simClients :: [Client],
    -- | Number of clients reading each stream (streams nobody reads are absent).
    subscribers :: Map Key Int
  }

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

-- | @mkPopulation clients epochsPerClient entries@. 'entries' is never empty
-- (the CLI requires at least one target entry).
mkPopulation :: Int -> Int -> V.Vector Entry -> Either Text Population
mkPopulation clients epochsPerClient entries = do
  perEntry <- traverse (clientStreams clients epochsPerClient) (V.toList entries)
  let simClients = map (Client . concat) (transpose perEntry)
      subscribers = Map.fromListWith (+) [(streamKey s, 1) | c <- simClients, s <- c.streams]
  pure Population {simClients, subscribers}

-- | For one entry: the streams it gives to each client (index = client number).
clientStreams :: Int -> Int -> Entry -> Either Text [[Stream]]
clientStreams clients epochs e = case e.pool of
  UserPool v -> Right [[StreamUser (v V.! (i `mod` s))] | i <- idxs]
  TeamPool v -> Right [[StreamTeam (v V.! (i `mod` s))] | i <- idxs]
  ClientsPool v ->
    let perUser = maybe 0 (NE.length . snd) (v V.!? 0)
     in if clients > s * perUser
          then Left ("clients: " <> n clients <> " clients need at least that many (user, client id) pairs, the pool has " <> n (s * perUser))
          else Right [[let (uid, cids) = v V.! (i `mod` s) in StreamClient uid (cids NE.!! (i `div` s))] | i <- idxs]
  EpochPool v
    | epochs > s -> Left ("epoch: --epochs-per-client " <> n epochs <> " exceeds the " <> n s <> " epoch streams")
    | otherwise ->
        Right [[uncurry StreamEpoch (v V.! ((i * epochs + j) `mod` s)) | j <- [0 .. epochs - 1]] | i <- idxs]
  ConnectionsPool _ -> Left "connections targets are not supported by consume"
  where
    s = e.spec.streams
    idxs = [0 .. clients - 1]
    n = T.pack . show

-- | Notifications that reach readers when this push succeeds.
expectedDeliveries :: Map Key Int -> FanInPush -> Int
expectedDeliveries subs push =
  sum [Map.findWithDefault 0 k subs | t <- push.targets, k <- targetKeys t]

-- | Per entry: share of its pool streams that have at least one reader.
coverage :: Map Key Int -> V.Vector Entry -> [(TargetKind, Double)]
coverage subs = map one . V.toList
  where
    one e =
      let covered = length [() | i <- [0 .. e.spec.streams - 1], any (`Map.member` subs) (targetKeys (targetAt e.pool i))]
       in (e.spec.kind, fromIntegral covered / fromIntegral e.spec.streams)
