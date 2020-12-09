{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ParseSchema where

import Data.Aeson (ToJSON, object, (.=))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TIO
import Imports
import qualified Options.Applicative as OA
import System.Environment (withArgs)
import System.Exit (exitFailure)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Mustache
import Text.RawString.QQ

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space space1 (L.skipLineComment ("--" :: Text)) empty)

data Column = Column
  { colName :: Text,
    colType :: Text
  }
  deriving (Eq, Show, Ord)

data CreateTable = CreateTable
  { ctKeyspace :: Text,
    ctTablename :: Text,
    ctColumns :: [Column]
  }
  deriving (Eq, Show, Ord)

name' :: Parser Text
name' = T.pack <$> some (satisfy (\c -> isLower c || c == '_'))

keyspace :: Parser Text
keyspace = name'

tablename :: Parser Text
tablename = keyspace

columnName :: Parser Text
columnName = name'

columnType :: Parser Text
columnType = T.pack <$> some (satisfy (\c -> isLower c || c == '_' || c == '<' || c == '>'))

column :: Parser Column
column = do
  col <- Column <$> lexeme columnName <*> lexeme columnType
  void $ optional (lexeme "PRIMARY KEY")
  pure col

betweenBrackets :: ParsecT Void Text Identity a -> ParsecT Void Text Identity a
betweenBrackets = between (lexeme "(") (lexeme ")")

primaryKeyAtEndOfColumns :: Parser ()
primaryKeyAtEndOfColumns = do
  void $ lexeme "PRIMARY KEY"
  void $
    lexeme $
      betweenBrackets $
        (void name' <|> nested) `sepBy1` lexeme ","
  where
    nested = void $ betweenBrackets $ name' `sepBy1` lexeme ","

createTable :: Parser CreateTable
createTable = do
  void $ lexeme "CREATE TABLE"
  ks <- keyspace
  void "."
  tn <- lexeme tablename
  cols <-
    betweenBrackets $
      catMaybes
        <$> ((Just <$> column) <|> (Nothing <$ primaryKeyAtEndOfColumns))
        `sepBy` lexeme ","
  noise
  pure (CreateTable ks tn cols)

noise :: Parser ()
noise = void $ some (satisfy (/= ';'))

stmt :: Parser (Maybe CreateTable)
stmt =
  do
    Just <$> createTable
    <|> (Nothing <$ (notFollowedBy (string "CREATE TABLE") >> noise))

parser :: Parser [CreateTable]
parser = catMaybes <$> (stmt `sepEndBy` lexeme (string ";"))

-------------------------------------------------------------------------------

toHaskellType :: Text -> Text
toHaskellType (T.split (`elem` ['<', '>']) -> ("frozen" : key')) = toHaskellType (mconcat key')
toHaskellType (T.split (`elem` ['<', '>']) -> ("list" : key')) = "[" <> toHaskellType (mconcat key') <> "]"
toHaskellType (T.split (`elem` ['<', '>']) -> ("set" : key')) = "[" <> toHaskellType (mconcat key') <> "]"
toHaskellType (T.splitOn "frozen" -> ["", key']) = toHaskellType key'
toHaskellType (T.splitOn "list" -> ["", key']) = "[" <> toHaskellType key' <> "]"
toHaskellType (T.splitOn "set" -> ["", key']) = "[" <> toHaskellType key' <> "]"
toHaskellType "ascii" = "Text"
toHaskellType "asset" = "TODO" -- this a cql data type consisting of two cols.  not sure we even need this.  hum.
toHaskellType "bigint" = "Integer"
toHaskellType "blob" = "LByteString"
toHaskellType "boolean" = "Bool"
toHaskellType "double" = "Double"
toHaskellType "float" = "Double"
toHaskellType "permissions" = "Integer"
toHaskellType "inet" = ""
toHaskellType "int" = "Integer"
toHaskellType "pubkey" = "Blob"
toHaskellType "text" = "Text"
toHaskellType "timestamp" = "UTCTime"
toHaskellType "timeuuid" = "UUID"
toHaskellType "uuid" = "UUID"
toHaskellType st = error (T.unpack ("toHaskellType not implemented for " <> st))

data TemplateValues = TemplateValues
  { tableName :: Text,
    tableId :: Text,
    columns :: Text,
    columnsQuoted :: Text,
    typeOfRow :: Text
  }
  deriving (Generic)

instance ToJSON TemplateValues

toTemplateValues :: CreateTable -> TemplateValues
toTemplateValues (CreateTable ks tn cols) =
  TemplateValues
    (ks' <> "." <> tn)
    (ks' <> "_" <> tn)
    (T.intercalate ", " (fmap colName cols))
    (T.intercalate ", " (fmap (quote . colName) cols))
    (T.intercalate ", " (fmap (("Maybe " <>) . toHaskellType . colType) cols))
  where
    ks' = fromMaybe ks (T.stripSuffix "_test" ks)
    quote :: Text -> Text
    quote str = "\"" <> str <> "\""

tableTemplate :: Text
tableTemplate =
  [r|
-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module {{moduleName}} where

type TODO = Void  -- TODO: remove me

{{#table}}

-- {{tableName}}

type Row_{{tableId}} = ({{{typeOfRow}}})

columns_{{tableId}} :: [Text]
columns_{{tableId}} = [{{{columnsQuoted}}}]

selectByTeam_{{tableId}} :: PrepQuery R (Identity TeamId) Row_{{tableId}}
selectByTeam_{{tableId}} = "select {{{columns}}} where team=?"
{{/table}}
|]

debug :: IO ()
debug =
  withArgs ["TableTypes", "../../../docs/reference/cassandra-schema.cql"] $
    main

data Arguments = Arguments
  { argsModuleName :: Text,
    argsCqlSchemaFile :: FilePath
  }

argParser :: OA.Parser Arguments
argParser =
  Arguments
    <$> OA.argument OA.str (OA.metavar "MODULE_NAME")
      <*> OA.argument OA.str (OA.metavar "SCHEMA_FILE")

main :: IO ()
main = do
  Arguments moduleName schemaFile <-
    OA.execParser
      ( OA.info
          (OA.helper <*> argParser)
          ( OA.header "gen-table-types"
              <> OA.progDesc "Generate Haskell types from Cassandra schema dump"
              <> OA.fullDesc
          )
      )
  contents <- T.readFile schemaFile
  createTables <- case parse parser "" contents of
    Left e -> putStr (errorBundlePretty e) >> exitFailure
    Right x -> pure x

  let res = compileMustacheText "" tableTemplate
  case res of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right template ->
      TIO.putStr $
        renderMustache template $
          object
            [ "table" .= fmap toTemplateValues createTables,
              "moduleName" .= moduleName
            ]
