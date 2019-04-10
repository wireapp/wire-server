
module Database.CQL.Parser () where

import Text.Megaparsec
import Text.Megaparsec.Char
import Imports
import Data.Text (pack)

example :: Text
example = "select id, user from user"
statement = Select [Field "id"] (TableName "user")

data Statement = Select [Field] TableName deriving (Show, Eq)

newtype Field = Field Text deriving (Show, Eq, Ord)
newtype TableName = TableName Text deriving (Show, Eq, Ord)


main :: IO ()
main = do
    putStrLn "I'm alive!"


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
pSelect = void $ string' "select" <* space

pFrom :: Parser ()
pFrom = void $ string' "from" <* space


pStatement :: Parser Statement 
pStatement = do
    pSelect
    fields <- pFields
    pFrom
    table <- pTableName
    pure $ Select fields table


func :: Text -> Statement
func input = either (error . errorBundlePretty) (id) (parse (pStatement <* space <* eof) "" input)


