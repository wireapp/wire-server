{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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
import System.FilePath ((</>))
import System.FilePath.Posix (takeDirectory)
import System.IO (Handle)
import System.Process (system)
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

-- TODO: the special cases in the first 6 lines should be handled by the parser, and this
-- function should get at least a [Text] with all the qualifiers and the base type. Or
-- something slightly more sophisticated.
toHaskellType :: Text -> Text
toHaskellType (T.split (`elem` ['<', '>']) -> ("frozen" : key')) = toHaskellType (mconcat key')
toHaskellType (T.split (`elem` ['<', '>']) -> ("list" : key')) = "[" <> toHaskellType (mconcat key') <> "]"
toHaskellType (T.split (`elem` ['<', '>']) -> ("set" : key')) = "(Cassandra.Set " <> toHaskellType (mconcat key') <> ")"
toHaskellType (T.splitOn "frozen" -> ["", key']) = toHaskellType key'
toHaskellType (T.splitOn "list" -> ["", key']) = "[" <> toHaskellType key' <> "]"
toHaskellType (T.splitOn "set" -> ["", key']) = "(Cassandra.Set " <> toHaskellType key' <> ")"
toHaskellType "ascii" = "Text"
toHaskellType "asset" = "TODO" -- this a cql data type consisting of two cols.  not sure we even need this.  hum.
toHaskellType "bigint" = "Integer"
toHaskellType "blob" = "LByteString"
toHaskellType "boolean" = "Bool"
toHaskellType "double" = "Double"
toHaskellType "float" = "Double"
toHaskellType "permissions" = "Permissions"
toHaskellType "inet" = "TODO"
toHaskellType "int" = "Int32"
toHaskellType "pubkey" = "Blob"
toHaskellType "text" = "Text"
toHaskellType "timestamp" = "UTCTime"
toHaskellType "timeuuid" = "UUID"
toHaskellType "uuid" = "UUID"
toHaskellType st = error (T.unpack ("toHaskellType not implemented for " <> st))

data Arguments = Arguments
  { argsCqlSchemaFile :: FilePath,
    argsCqlOutputFile :: Maybe FilePath
  }

argParser :: OA.Parser Arguments
argParser =
  Arguments
    <$> OA.argument OA.str (OA.metavar "SCHEMA_FILE")
    <*> ( optional $
            OA.strOption
              ( OA.long "output"
                  <> OA.short 'o'
                  <> OA.metavar "FILE"
                  <> OA.help "Write output to FILE"
              )
        )

moduleTemplate :: Text
moduleTemplate =
  [r|{-# LANGUAGE RecordWildCards #-}

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

import Cassandra
import Data.Conduit
import Data.Id
import Data.Time
import Data.UUID
import Galley.Data.Instances ()
import Imports
import Types
import Wire.API.Team.Permission

-- This file was autogenerated by gen-table-types

type TODO = Int32 -- TODO: remove

{{#chunkTable}}

-- {{keySpace}}.{{tableName}}

type Row{{keySpaceCaml}}{{tableNameCaml}} = ({{{typeOfRow}}})

select{{keySpaceCaml}}{{tableNameCaml}} :: PrepQuery R ({{lookupParamsType}}) Row{{keySpaceCaml}}{{tableNameCaml}}
select{{keySpaceCaml}}{{tableNameCaml}} = "SELECT {{columns}} FROM {{tableName}} WHERE {{lookupKeyWhere}} ALLOW FILTERING"

read{{keySpaceCaml}}{{tableNameCaml}} :: Env -> {{lookupKeyType}} -> IO [Row{{keySpaceCaml}}{{tableNameCaml}}]
read{{keySpaceCaml}}{{tableNameCaml}} Env {..} {{lookupKeyVar}} =
  runClient env{{keySpaceCaml}} $
    retry x1 (query select{{keySpaceCaml}}{{tableNameCaml}} (params Quorum ({{lookupParamsTerm}})))

read{{keySpaceCaml}}{{tableNameCaml}}Conduit :: Env -> {{lookupKeyType}} -> ConduitM () [Row{{keySpaceCaml}}{{tableNameCaml}}] IO ()
read{{keySpaceCaml}}{{tableNameCaml}}Conduit Env {..} {{lookupKeyVar}} =
  transPipe (runClient env{{keySpaceCaml}}) $
    paginateC select{{keySpaceCaml}}{{tableNameCaml}} (paramsP Quorum (pure {{lookupKeyVar}}) envPageSize) x5

insert{{keySpaceCaml}}{{tableNameCaml}} :: Env -> FilePath -> IO ()
insert{{keySpaceCaml}}{{tableNameCaml}} _ _ = do
  -- TODO:
  -- if file does not exist, do nothing.
  -- otherwise, read lines from file;
  -- parse each as 'Row{{keySpaceCaml}}{{tableNameCaml}}';
  -- run Client action on each.
  pure ()
{{/chunkTable}}

insertAllTables :: Env -> IO ()
insertAllTables env = do
{{#chunkTable}}
  insert{{keySpaceCaml}}{{tableNameCaml}} env "{{keySpace}}.{{tableName}}"
{{/chunkTable}}
|]

_printAllTables :: [CreateTable] -> IO ()
_printAllTables createTables =
  for_ createTables $ \(CreateTable {..}) ->
    putStrLn $ T.unpack (ctKeyspace <> "." <> ctTablename)

main :: IO ()
main = do
  Arguments schemaFile mbOutFile <-
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

  let mkChunk' ks tn lookupKeyType lookupKeyVar lookupKeyWhere =
        mkChunk createTables ks tn lookupKeyType lookupKeyVar lookupKeyWhere ("Identity " <> lookupKeyType) ("pure " <> lookupKeyVar)

      mkChunkUsers ks tn =
        mkChunk createTables ks tn "[UserId]" "uids" "user in ?" "Identity ([UserId])" "pure uids"

      mkChunkTeam ks tn =
        mkChunk createTables ks tn "TeamId" "tid" "team = ?" "Identity TeamId" "pure tid"

  withOutputHandle mbOutFile $ \out ->
    case compileMustacheText "" moduleTemplate of
      Left bundle -> putStrLn (errorBundlePretty bundle)
      Right template -> do
        (TIO.hPutStr out . renderMustache template)
          ( object
              [ "chunkTable"
                  .= [ mkChunkUsers "brig" "activation_keys",
                       -- "brig" "blacklist"
                       -- "brig" "budget"
                       mkChunkUsers "brig" "clients",
                       mkChunkUsers "brig" "codes",
                       mkChunk createTables "brig" "connection" "[UserId]" "uids" "left in ? OR right in ?" "([UserId], [UserId])" "(uids, uids)",
                       -- "brig" "excluded_phones",
                       mkChunk' "brig" "id_mapping" "[UserId]" "uids" "mapped_id in",
                       mkChunk' "brig" "invitation" "[UserId]" "uids" "inviter in",
                       mkChunk' "brig" "invitation_info" "[UserId]" "uids" "inviter in",
                       mkChunk createTables "brig" "invitee_info" "[UserId]" "uids" "invitee in ? OR inviter in ?" "([UserId], [UserId])" "(uids, uids)",
                       -- mkChunk createTables "brig" "login_codes" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "meta" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "password_reset" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "prekeys" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "properties" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "provider" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "provider_keys" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "rich_info" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service_prefix" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service_tag" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service_team" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service_user" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service_whitelist" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "service_whitelist_rev" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "team_invitation" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "team_invitation_email" undefined undefined undefined undefined undefined,
                       -- mkChunkTeam "brig" "team_invitation_info",
                       -- mkChunk createTables "brig" "unique_claims" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "user" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "user_cookies" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "user_handle" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "user_keys" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "user_keys_hash" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "brig" "vcodes" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "billing_team_member" undefined undefined undefined undefined undefined,
                       mkChunkUsers "galley" "clients",
                       -- mkChunk createTables "galley" "conversation" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "conversation_codes" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "custom_backend" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "data_migration" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "id_mapping" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "legalhold_pending_prekeys" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "legalhold_service" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "member" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "meta" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "service" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "team" undefined undefined undefined undefined undefined,
                       mkChunkTeam "galley" "team_conv",
                       -- mkChunk createTables "galley" "team_conv" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "team_features" undefined undefined undefined undefined undefined,
                       mkChunkTeam "galley" "team_member"
                       -- mkChunk createTables "galley" "team_notifications" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "user" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "galley" "user_team" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "gundeck" "meta" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "gundeck" "notifications" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "gundeck" "push" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "gundeck" "user_push" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "authreq" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "authresp" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "bind_cookie" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "default_idp" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "idp" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "idp_raw_metadata" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "issuer_idp" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "meta" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "scim_external_ids" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "scim_user_times" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "team_idp" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "team_provisioning_by_team" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "team_provisioning_by_token" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "user" undefined undefined undefined undefined undefined,
                       -- mkChunk createTables "spar" "verdict" undefined undefined undefined undefined undefined
                     ]
              ]
          )

  for_ mbOutFile $ \filename ->
    system $ "ormolu --mode inplace " <> filename
  where
    withOutputHandle :: Maybe FilePath -> (Handle -> IO ()) -> IO ()
    withOutputHandle mbOutFile fn = case mbOutFile of
      Nothing -> fn stdout
      Just filename -> withFile filename WriteMode fn

findFileUpwards :: FilePath -> FilePath -> IO (Maybe FilePath)
findFileUpwards startDir =
  findFile (fmap (nthParent startDir) [0 .. 10])
  where
    nthParent :: FilePath -> Int -> FilePath
    nthParent dir i =
      foldl' (</>) dir (replicate i "..")

findProjectRoot :: IO FilePath
findProjectRoot = do
  curDir <- getCurrentDirectory
  Just p <- findFileUpwards curDir "stack-deps.nix"
  pure $ takeDirectory p

projectFile :: FilePath -> IO FilePath
projectFile relativeFilename =
  (</> relativeFilename) <$> findProjectRoot

debug :: IO ()
debug = do
  cassandraSchema <- projectFile "docs/reference/cassandra-schema.cql"
  withArgs [cassandraSchema] main

debugwrite :: IO ()
debugwrite = do
  cassandraSchema <- projectFile "docs/reference/cassandra-schema.cql"
  outputFile <- projectFile "tools/db/move-team/src/Schema.hs"
  withArgs [cassandraSchema, "--output=" <> outputFile] main

data Chunk = Chunk
  { lookupKeyType :: Text,
    lookupKeyVar :: Text,
    lookupKeyWhere :: Text,
    lookupParamsType :: Text,
    lookupParamsTerm :: Text,
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

mkChunk :: [CreateTable] -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Chunk
mkChunk createTables ks tn lookupKeyType lookupKeyVar lookupKeyWhere lookupParamsType lookupParamsTerm =
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
