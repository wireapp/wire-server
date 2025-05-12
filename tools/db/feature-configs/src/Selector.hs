module Selector where

import Data.Attoparsec.Text
import Data.Char qualified as Char
import Data.Scientific (Scientific)
import Data.Text qualified as Text
import Imports
import Wire.API.Team.Feature

data Selector
  = SelectorFeatureStatusEq FeatureStatus
  | SelectorLockStatusEq LockStatus
  | SelectorDbConfigCompare [Text] Ordering Val
  | SelectorAnd Selector Selector
  | SelectorOr Selector Selector
  deriving (Show)

data Val = ValNum Scientific | ValStr Text
  deriving (Show, Eq, Ord)

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

dbConfigSelectorParser :: Parser ([Text], Ordering, Val)
dbConfigSelectorParser = do
  _ <- string "config"
  _ <- char '.'
  let pathSegmentParser = fmap Text.pack . many1' . satisfy $ \c ->
        not (Char.isSpace c) -- Avoid complications of spaces in paths
          && not (inClass ".>=<\"" c) -- Avoid parsing symbols which may come
          -- after the path, no support for escapes here. Hope we don't make
          -- complicated configs.
  path <- sepBy1' pathSegmentParser (char '.')
  op <- (EQ <$ char '=') <|> (GT <$ char '>') <|> (LT <$ char '<')
  val <-
    (ValNum <$> signed scientific)
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
  | UpdateMultiple UpdateOperation UpdateOperation
  deriving (Show)

parseUpdateOperation :: String -> Either String UpdateOperation
parseUpdateOperation input = parseOnly (updateOperationParser <* endOfInput) (Text.pack input)

updateOperationParser :: Parser UpdateOperation
updateOperationParser = do
  upd1 <-
    (UpdateStatus <$> featureStatusParser)
      <|> (UpdateLockStatus <$> lockStatusParser)
      <|> ( dbConfigSelectorParser >>= \(path, op, val) -> do
              when (op /= EQ) $ fail $ "Invalid use of non '=' operator: " <> show op
              pure $ UpdateDbConfig path val
          )
  _ <- skipSpace
  c <- peekChar
  case c of
    Just ',' -> do
      _ <- char ','
      _ <- skipSpace
      UpdateMultiple upd1 <$> updateOperationParser
    _ -> pure upd1
