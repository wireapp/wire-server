module Selector where

import Data.Attoparsec.Text
import Data.Char qualified as Char
import Data.Text qualified as Text
import Imports
import Wire.API.Team.Feature

data Selector
  = SelectorFeatureStatusEq FeatureStatus
  | SelectorLockStatusEq LockStatus
  | SelectorDbConfigCompare [Text] Operator Val
  | SelectorAnd Selector Selector
  | SelectorOr Selector Selector
  deriving (Show)

data Operator = OperatorEq | OperatorGT | OperatorLT
  deriving (Show, Eq)

data Val = ValNum Int32 | ValStr Text
  deriving (Show)

parseSelector :: String -> Either String Selector
parseSelector input = parseOnly (selectorParser <* endOfInput) (Text.pack input)

-- | Doesn't support brackets, maybe do it later.
selectorParser :: Parser Selector
selectorParser = do
  sel1 <-
    (SelectorFeatureStatusEq <$> featureStatusParser)
      <|> (SelectorLockStatusEq <$> lockStatusParser)
      <|> (dbConfigSelectorParser <&> \(path, op, val) -> SelectorDbConfigCompare path op val)
  _ <- skipSpace
  c <- peekChar
  case c of
    Just '&' -> do
      _ <- string "&&"
      _ <- skipSpace
      SelectorAnd sel1 <$> selectorParser
    Just '|' -> do
      _ <- string "||"
      _ <- skipSpace
      SelectorOr sel1 <$> selectorParser
    _ -> pure sel1

featureStatusParser :: Parser FeatureStatus
featureStatusParser = do
  _ <- string "status"
  _ <- skipSpace
  _ <- char '='
  _ <- skipSpace
  (FeatureStatusEnabled <$ string "enabled")
    <|> (FeatureStatusDisabled <$ string "disabled")

lockStatusParser :: Parser LockStatus
lockStatusParser = do
  _ <- string "lockStatus"
  _ <- skipSpace
  _ <- char '='
  _ <- skipSpace
  (LockStatusLocked <$ string "locked")
    <|> (LockStatusUnlocked <$ string "unlocked")

dbConfigSelectorParser :: Parser ([Text], Operator, Val)
dbConfigSelectorParser = do
  _ <- string "config"
  _ <- char '.'
  let pathSegmentParser = fmap Text.pack . many1' . satisfy $ \c ->
        not (Char.isSpace c) -- Avoid complications of spaces in paths
          && not (Char.isSymbol c) -- Avoid parsing . > = < along with other
          -- symbols which are probably not going to be
          -- in the config
  path <- sepBy1' pathSegmentParser (char '.')
  op <- (OperatorEq <$ char '=') <|> (OperatorGT <$ char '>') <|> (OperatorLT <$ char '<')
  val <-
    (ValNum <$> signed decimal)
      <|> ( ValStr <$> do
              _ <- char '"'
              str <- Text.pack <$> many1' (satisfy (\c -> c /= '"'))
              _ <- char '"'
              pure str
          )
  pure $ (path, op, val)

data UpdateOperation
  = UpdateStatus FeatureStatus
  | UpdateLockStatus LockStatus
  | UpdateDbConfig [Text] Val
  deriving (Show)

parseUpdateOperation :: String -> Either String UpdateOperation
parseUpdateOperation input = parseOnly (updateOperationParser <* endOfInput) (Text.pack input)

updateOperationParser :: Parser UpdateOperation
updateOperationParser =
  (UpdateStatus <$> featureStatusParser)
    <|> (UpdateLockStatus <$> lockStatusParser)
    <|> ( dbConfigSelectorParser >>= \(path, op, val) -> do
            unless (op /= OperatorEq) $ fail "Invalid use of non '=' operator"
            pure $ UpdateDbConfig path val
        )
