{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ParseSchema where

import Data.Aeson (ToJSON, object, toJSON, (.=))
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

findCreateTable :: Text -> Text -> [CreateTable] -> CreateTable
findCreateTable ks tn [] = error $ "bad CreateTable name, keyspace: " <> show (ks, tn)
findCreateTable ks tn (ct@(CreateTable ks' tn' _) : cts) =
  if ks == ks' && tn == tn'
    then ct
    else findCreateTable ks tn cts

name' :: Parser Text
name' = T.pack <$> some (satisfy (\c -> isLower c || c == '_'))

keyspace :: Parser Text
keyspace = do
  n <- name'
  pure $ case T.splitOn "_test" n of
    [n', ""] -> n'
    _ -> n

tablename :: Parser Text
tablename = name'

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

module Schema where

{{#chunkTable}}
-- {{keySpace}}.{{tableName}}

type Row{{keySpaceCaml}}{{tableNameCaml}} = ({{{typeOfRow}}})

select{{keySpaceCaml}}{{tableNameCaml}} :: PrepQuery R (Identity {{lookupKeyType}}) Row{{keySpaceCaml}}{{tableNameCaml}}
select{{keySpaceCaml}}{{tableNameCaml}} = "select {{columns}} from {{tableNameCaml}} where {{lookupKeyCol}} = ?"

read{{keySpaceCaml}}{{tableNameCaml}} :: {{lookupKeyType}} -> Client [Row{{keySpaceCaml}}{{tableNameCaml}}]
read{{keySpaceCaml}}{{tableNameCaml}} {{lookupKeyType}} =
  retry x1 (query1 select{{keySpaceCaml}}{{tableNameCaml}} (params Quorum (pure {{lookupKeyType}})))

read{{keySpaceCaml}}{{tableNameCaml}}Conduit :: {{lookupKeyType}} -> ConduitM () [Row{{keySpaceCaml}}{{tableNameCaml}}] Client ()
read{{keySpaceCaml}}{{tableNameCaml}}Conduit {{lookupKeyType}} =
  paginateC select{{keySpaceCaml}}{{tableNameCaml}} (paramsP Quorum (pure {{lookupKeyType}}) pageSize) x5

{{/chunkTable}}
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

  case compileMustacheText "" tableTemplate of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right template -> do
      (TIO.putStr . renderMustache template)
        ( object
            [ "chunkTable"
                .= [ mkChunk createTables "galley" "team_member" "TeamId" "tid" "team",
                     mkChunk createTables "galley" "team_conv" "TeamId" "tid" "team"
                   ]
            ]
        )

data Chunk = Chunk
  { lookupKeyType :: Text,
    lookupKeyVar :: Text,
    lookupKeyCol :: Text,
    keySpace :: Text,
    tableName :: Text,
    keySpaceCaml :: Text,
    tableNameCaml :: Text,
    columns :: Text,
    columnsQuoted :: Text,
    typeOfRow :: Text
  }
  deriving (Generic)

instance ToJSON Chunk

mkChunk :: [CreateTable] -> Text -> Text -> Text -> Text -> Text -> Chunk
mkChunk createTables ks tn lookupKeyType lookupKeyVar lookupKeyCol =
  let (CreateTable _ks _tn cols) = findCreateTable ks tn createTables
      keySpace = ks
      tableName = tn
      keySpaceCaml = caml ks
      tableNameCaml = caml tn
      columns = T.intercalate ", " (fmap colName cols)
      columnsQuoted = T.intercalate ", " (fmap (quote . colName) cols)
      typeOfRow = T.intercalate ", " (fmap (("Maybe " <>) . toHaskellType . colType) cols)
   in Chunk {..}
  where
    caml :: Text -> Text
    caml = T.pack . go 0 . T.unpack
      where
        go _ ('_' : c : cs) = toUpper c : go 1 cs
        go 0 (c : cs) = toUpper c : go 1 cs
        go _ (c : cs) = c : go 1 cs
        go _ "" = ""

    quote :: Text -> Text
    quote str = "\"" <> str <> "\""
