module FanInPerf.TargetSpecParser
  ( parseTargetSpec,
  )
where

import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty (NonEmpty (..))
import FanInPerf.Targets (TargetConfig (..), TargetKind, allKinds, kindName)
import Imports
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | Parses the value of the @--targets@ option.
--
-- Grammar:
--
-- @
-- spec  ::= entry (',' entry)*   -- no kind may occur twice
-- entry ::= kind ':' streams ('x' perPush)?
-- kind  ::= \"user\" | \"clients\" | \"team\" | \"epoch\" | \"connections\"
-- streams, perPush ::= decimal number, 1 .. 10_000_000
-- @
--
-- @perPush@ defaults to 1 and must not exceed @streams@.
--
-- Mapping of an example to 'TargetConfig' fields from CLI:
--
-- @
-- --targets user:1000x5,team:10
--            │    │   │
--            │    │   └ perPush K=5: targets per push (default 1)
--            │    └ streams N=1000: distinct keys in pool
--            └ kind
-- @
parseTargetSpec :: Text -> Either String (NonEmpty TargetConfig)
parseTargetSpec = first errorBundlePretty . parse (specP <* eof) "--targets"
  where
    specP :: Parser (NonEmpty TargetConfig)
    specP = do
      e <- entryP
      es <- many (char ',' *> entryP)
      let entries = e :| es
          kinds = map (.kind) (e : es)
      when (length (nubOrd kinds) /= length kinds) $
        fail "duplicate target kind"
      pure entries

    entryP :: Parser TargetConfig
    entryP = do
      kind <- kindP
      _ <- char ':'
      streams <- countP
      perPush <- fromMaybe 1 <$> optional (char 'x' *> countP)
      when (perPush > streams) $
        fail ("targets per push exceed streams: " <> show perPush <> " > " <> show streams)
      pure TargetConfig {..}

    kindP :: Parser TargetKind
    kindP = choice [k <$ string (kindName k) | k <- allKinds] <?> "target kind"

    -- \| Parsed as 'Integer' first so huge inputs cannot overflow 'Int'.
    countP :: Parser Int
    countP = label "number" $ do
      n <- L.decimal :: Parser Integer
      when (n < 1 || n > toInteger maxStreams) $
        fail ("expected a number between 1 and " <> show maxStreams <> ", got: " <> show n)
      pure (fromInteger n)

    -- \| Stream keys are kept in memory, so their number is bounded.
    maxStreams :: Int
    maxStreams = 10_000_000
