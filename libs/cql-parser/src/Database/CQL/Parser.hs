module Database.CQL.Parser  where

import Text.Megaparsec
import Text.Megaparsec.Char
import Imports
import Data.Text (pack)
import Data.Bifunctor

example :: Text
example = " sElEcT id, user fRom user  "

statement :: Statement
statement = Select [Field "id"] (TableName "user")

data Statement = Select [Field] TableName deriving (Show, Eq)

newtype Field = Field Text deriving (Show, Eq, Ord)
newtype TableName = TableName Text deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

pField :: Parser Field
pField = do
    s :: [Char] <- some (alphaNumChar <|> (char '_'))
    pure $ Field (pack s)

pTableName :: Parser TableName
pTableName = do
    s :: [Char] <- some (alphaNumChar <|> (char '_'))
    pure $ TableName (pack s)

pFields :: Parser [Field]
pFields = pField `sepBy` (char ',' <* space) <* space

pSelect :: Parser ()
pSelect = void $ string' "select" <* space1

pFrom :: Parser ()
pFrom = void $ string' "from" <* space1


pStatement :: Parser Statement
pStatement = do
    space
    pSelect
    fields <- pFields
    pFrom
    table <- pTableName
    pure $ Select fields table


func :: Text -> Statement
func input = either (error . errorBundlePretty) (id) (parse (pStatement <* space <* eof) "" input)

parseCql :: Text -> Either String Statement
parseCql input = first errorBundlePretty $ parse (pStatement <* space <* eof) "" input
