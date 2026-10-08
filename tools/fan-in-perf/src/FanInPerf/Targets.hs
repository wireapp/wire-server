module FanInPerf.Targets
  ( TargetKind (..),
    allKinds,
    kindName,
    TargetEntry (..),
    maxStreams,
    parseTargetSpec,
    renderTargetSpec,
  )
where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Imports

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
