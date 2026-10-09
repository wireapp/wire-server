module FanInPerf.TargetSpecParser
  ( parseTargetSpec,
  )
where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Read qualified as T
import FanInPerf.Targets (TargetConfig (..), TargetKind, allKinds, kindName)
import Imports

parseTargetSpec :: Text -> Either String (NonEmpty TargetConfig)
parseTargetSpec spec = do
  entries <- traverse parseEntry (T.splitOn "," spec)
  let kinds = map (.kind) entries
  when (Set.size (Set.fromList kinds) /= length kinds) $
    Left "duplicate target kind"
  maybe (Left "empty target spec") Right (nonEmpty entries)
  where
    parseEntry :: Text -> Either String TargetConfig
    parseEntry entry = case T.splitOn ":" entry of
      [k, counts] -> do
        kind <- parseKind k
        (streams, perPush) <- case T.splitOn "x" counts of
          [s] -> (,1) <$> parseCount s
          [s, p] -> (,) <$> parseCount s <*> parseCount p
          _ -> malformed entry
        when (perPush > streams) $
          Left ("targets per push exceed streams: " <> T.unpack entry)
        pure TargetConfig {..}
      _ -> malformed entry

    malformed :: Text -> Either String a
    malformed entry = Left ("malformed target entry (expected KIND:STREAMS[xK]): " <> T.unpack entry)

    parseKind :: Text -> Either String TargetKind
    parseKind t =
      maybe (Left ("unknown target kind: " <> T.unpack t)) Right $
        find ((== t) . kindName) allKinds

    -- Parsed as 'Integer' first so huge inputs cannot overflow 'Int'.
    parseCount :: Text -> Either String Int
    parseCount t = case T.decimal @Integer t of
      Right (n, rest)
        | T.null rest && n > 0 && n <= toInteger maxStreams -> Right (fromInteger n)
      _ -> Left ("expected a number between 1 and " <> show maxStreams <> ", got: " <> T.unpack t)

    -- Stream keys are kept in memory, so their number is bounded.
    maxStreams :: Int
    maxStreams = 10_000_000
