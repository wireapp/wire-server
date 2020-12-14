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
toHaskellType "ascii" = "Ascii"
toHaskellType "asset" = "AssetIgnoreData" -- TODO: check if we really dont need data of this type
toHaskellType "bigint" = "Int64"
toHaskellType "blob" = "Blob"
toHaskellType "boolean" = "Bool"
toHaskellType "double" = "Double"
toHaskellType "float" = "Float"
toHaskellType "permissions" = "Permissions"
toHaskellType "inet" = "IP"
toHaskellType "int" = "Int32"
toHaskellType "pubkey" = "Blob"
toHaskellType "text" = "Text"
toHaskellType "timestamp" = "UTCTime"
toHaskellType "timeuuid" = "TimeUuid"
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
import Common
import Data.Conduit
import Data.Id
import Data.Time
import Data.UUID
import Galley.Data.Instances ()
import Imports
import Types
import Wire.API.Team.Permission
import Data.IP (IP)
import Data.Handle (Handle)
import Wire.API.User.Password (PasswordResetKey)
import System.FilePath.Posix ((</>))

-- This file was autogenerated by gen-table-types


{{#chunkTable}}

-- {{keySpace}}.{{tableName}}

type Row{{keySpaceCaml}}{{tableNameCaml}} = ({{{typeOfRow}}})

select{{keySpaceCaml}}{{tableNameCaml}} :: PrepQuery R (Identity ({{lookupKeyType}})) Row{{keySpaceCaml}}{{tableNameCaml}}
select{{keySpaceCaml}}{{tableNameCaml}} = "SELECT {{columns}} FROM {{tableName}} WHERE {{lookupKeyWhere}}"

read{{keySpaceCaml}}{{tableNameCaml}}:: Env -> {{lookupKeyType}} -> ConduitM () [Row{{keySpaceCaml}}{{tableNameCaml}}] IO ()
read{{keySpaceCaml}}{{tableNameCaml}} Env {..} {{lookupKeyVar}} =
  transPipe (runClient env{{keySpaceCaml}}) $
    paginateC select{{keySpaceCaml}}{{tableNameCaml}} (paramsP Quorum (pure {{lookupKeyVar}}) envPageSize) x5

select{{keySpaceCaml}}{{tableNameCaml}}All :: PrepQuery R () Row{{keySpaceCaml}}{{tableNameCaml}}
select{{keySpaceCaml}}{{tableNameCaml}}All = "SELECT {{columns}} FROM {{tableName}}"

read{{keySpaceCaml}}{{tableNameCaml}}ConduitAll :: Env -> ConduitM () [Row{{keySpaceCaml}}{{tableNameCaml}}] IO ()
read{{keySpaceCaml}}{{tableNameCaml}}ConduitAll Env {..} =
  transPipe (runClient env{{keySpaceCaml}}) $
    paginateC select{{keySpaceCaml}}{{tableNameCaml}}All (paramsP Quorum () envPageSize) x5

export{{keySpaceCaml}}{{tableNameCaml}}Full :: Env -> FilePath -> IO ()
export{{keySpaceCaml}}{{tableNameCaml}}Full env@Env {..} path = do
  putStrLn $ "Exporting " <> "{{keySpace}}.{{tableName}}"  <> " to " <> path
  withBinaryFile path WriteMode $ \handle ->
    runConduit $
      read{{keySpaceCaml}}{{tableNameCaml}}ConduitAll env
        .| sinkJsonLines handle

insert{{keySpaceCaml}}{{tableNameCaml}} :: PrepQuery W Row{{keySpaceCaml}}{{tableNameCaml}} ()
insert{{keySpaceCaml}}{{tableNameCaml}} =
    "INSERT INTO {{tableName}} ({{columns}}) VALUES {{placeHolders}}"

import{{keySpaceCaml}}{{tableNameCaml}} :: Env -> FilePath -> IO ()
import{{keySpaceCaml}}{{tableNameCaml}} Env{..} path = do
  exists <- doesFileExist path
  if exists then do
    putStrLn $ "Importing " <> path <> " to " <> "{{keySpace}}.{{tableName}}"
    withBinaryFile path ReadMode $ \handle -> do
      runConduit $
        sourceJsonLines handle
          .| transPipe (runClient env{{keySpaceCaml}}) (sinkTableRows insert{{keySpaceCaml}}{{tableNameCaml}})
  else do
     putStrLn $ "Skipping because not found: " <> path
     pure ()
{{/chunkTable}}

importAllTables :: Env -> IO ()
importAllTables env@Env {..} = do
{{#chunkTable}}
  import{{keySpaceCaml}}{{tableNameCaml}} env (envTargetPath </> "{{keySpace}}.{{tableName}}")
{{/chunkTable}}

exportAllTablesFull :: Env -> IO ()
exportAllTablesFull env@Env {..} = do
{{#chunkTable}}
  export{{keySpaceCaml}}{{tableNameCaml}}Full env (envTargetPath </> "{{keySpace}}.{{tableName}}")
{{/chunkTable}}
|]

_printAllTables :: [CreateTable] -> IO ()
_printAllTables createTables =
  for_ createTables $ \CreateTable {..} ->
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

  let mkChunk' = mkChunk createTables

      mkChunkUsers ks tn =
        mkChunk createTables ks tn "[UserId]" "uids" "user in ?"

      mkChunkTeam ks tn =
        mkChunk createTables ks tn "TeamId" "tid" "team = ?"

  withOutputHandle mbOutFile $ \out ->
    case compileMustacheText "" moduleTemplate of
      Left bundle -> putStrLn (errorBundlePretty bundle)
      Right template -> do
        (TIO.hPutStr out . renderMustache template)
          ( object
              [ "chunkTable"
                  .= [ -- mkChunkUsers "brig" "activation_keys",
                       -- "brig" "blacklist"
                       -- "brig" "budget"
                       -- PRIMARY KEY (user, client)
                       mkChunkUsers "brig" "clients",
                       -- mkChunkUsers "brig" "codes",
                       -- PRIMARY KEY (left, right) -- TODO: is this symmetric? At first glance it looks like yes
                       mkChunk' "brig" "connection" "[UserId]" "uids" "left in ?",
                       -- "brig" "excluded_phones",
                       -- PRIMARY KEY (mapped_id) -- TODO: is mapped_id a user id?
                       mkChunk' "brig" "id_mapping" "[UserId]" "uids" "mapped_id in ?",
                       -- mkChunk' "brig" "invitation" "[UserId]" "uids" "inviter in ?",
                       -- mkChunk' "brig" "invitation_info" "[UserId]" "uids" "inviter in ?",
                       -- mkChunk createTables "brig" "invitee_info" "[UserId]" "uids" "invitee in ? OR inviter in ?" "([UserId], [UserId])" "(uids, uids)",
                       -- PRIMARY KEY (user)
                       mkChunkUsers "brig" "login_codes", -- TODO: do we need this?
                       -- "brig" "meta",
                       -- PRIMARY KEY(key) -- TODO: find connection
                       mkChunk' "brig" "password_reset" "[PasswordResetKey]" "reset_keys" "key in ?", -- ? probably not.
                       -- PRIMARY KEY (user, client, key)
                       mkChunkUsers "brig" "prekeys", -- TODO: do new need this ?
                       -- PRIMARY KEY (user, key)
                       mkChunkUsers "brig" "properties",
                       -- "brig" "provider",
                       -- "brig" "provider_keys",
                       -- PRIMARY KEY (user)
                       mkChunkUsers "brig" "rich_info",
                       -- "brig" "service"
                       -- "brig" "service_prefix"
                       -- "brig" "service_tag"
                       -- "brig" "service_team"
                       -- "brig" "service_user"
                       -- "brig" "service_whitelist"
                       -- "brig" "service_whitelist_rev"
                       -- mkChunkTeam "brig" "team_invitation",
                       -- mkChunkTeam "brig" "team_invitation_email",
                       -- mkChunk' "brig" "team_invitation_info" "[UserId]" "uids" "inviter in ?",
                       -- "brig" "unique_claims"
                       -- PRIMARY KEY (id)
                       mkChunk' "brig" "user" "[UserId]" "uids" "id in ?",
                       -- mkChunkUsers "brig" "user_cookies",
                       -- PRIMARY KEY (handle :: text) -- TODO: get from brig.user.handle
                       mkChunk' "brig" "user_handle" "[Handle]" "handles" "handle in ?",
                       -- brig.user_keys: TODO: do we need it? find connection
                       -- PRIMARY KEY (key :: text)
                       -- See Brig.Data.UserKey
                       -- mkChunkUsers "brig" "user_keys" "[Text]" "keys" "key in ?",  -- ?
                       --
                       -- brig.user_keys_hash -- TODO: do we need it? find connection
                       -- PRIMARY KEY (key :: blob)
                       -- mkChunk' "brig" "user_keys_hash" "[UserKeyHash]" "keys" "key in ?"
                       -- "brig" "vcodes"
                       -- PRIMARY KEY (team, user)
                       mkChunkTeam "galley" "billing_team_member",
                       -- PRIMARY KEY (user)
                       mkChunkUsers "galley" "clients",
                       -- PRIMARY KEY (conv)
                       mkChunk' "galley" "conversation" "[ConvId]" "cids" "conv in ?",
                       -- "galley" "conversation_codes",
                       -- "galley" "custom_backend",
                       -- "galley" "data_migration",
                       -- "galley" "id_mapping",
                       -- mkChunkUsers "galley" "legalhold_pending_prekeys",
                       -- mkChunkTeam "galley" "legalhold_service",
                       -- PRIMARY KEY (conv, user) -- TODO: check how connect via galley.team_conv
                       mkChunk' "galley" "member" "[ConvId]" "cids" "conv in ?",
                       -- "galley" "meta"
                       -- "galley" "service"
                       -- PRIMARY KEY (team)
                       mkChunkTeam "galley" "team",
                       -- PRIMARY KEY (team, conv)
                       mkChunkTeam "galley" "team_conv",
                       -- PRIMARY KEY (team_id)
                       mkChunk' "galley" "team_features" "TeamId" "tid" "team_id = ?",
                       -- PRIMARY KEY (team, user)
                       mkChunkTeam "galley" "team_member",
                       -- PRIMARY KEY (team, id)
                       mkChunkTeam "galley" "team_notifications",
                       -- PRIMARY KEY (user, conv)
                       mkChunkUsers "galley" "user",
                       -- PRIMARY KEY (user, team)
                       mkChunkUsers "galley" "user_team",
                       -- "gundeck" "meta",
                       -- PRIMARY KEY (user, id)
                       mkChunkUsers "gundeck" "notifications",
                       -- mkChunk' "gundeck" "push" "[UserId]" "uids" "usr in ?",
                       -- mkChunk' "gundeck" "user_push" "[UserId]" "uids" "usr in ?",
                       -- "spar" "authreq",
                       -- "spar" "authresp",
                       -- mkChunk' "spar" "bind_cookie" "[UserId]" "uids" "session_owner in ?",
                       -- "spar" "default_idp"
                       -- mkChunkTeam "spar" "idp",
                       -- "spar" "idp_raw_metadata",
                       -- "spar" "issuer_idp",
                       -- "spar" "meta",
                       -- PRIMARY KEY (external :: text) -- TODO: find connection OR maybe full table scan?
                       -- mkChunkUsers "spar" "scim_external_ids",
                       -- PRIMARY KEY (uid)
                       mkChunk' "spar" "scim_user_times" "[UserId]" "uids" "uid in ?"
                       -- mkChunkTeam "spar" "team_idp",
                       -- mkChunkTeam "spar" "team_provisioning_by_team",
                       -- mkChunkTeam "spar" "team_provisioning_by_token",
                       -- mkChunkUsers "spar" "user"
                       -- "spar" "verdict"
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

-- | FUTUREWORK: can be implemented more easily with `stack path --project-root`
projectFile :: FilePath -> IO FilePath
projectFile relativeFilename =
  (</> relativeFilename) <$> findProjectRoot
  where
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
    keySpace :: Text,
    tableName :: Text,
    keySpaceCaml :: Text,
    tableNameCaml :: Text,
    columns :: Text,
    columnsQuoted :: Text,
    typeOfRow :: Text,
    placeHolders :: Text
  }
  deriving (Generic)

instance ToJSON Chunk

mkChunk :: [CreateTable] -> Text -> Text -> Text -> Text -> Text -> Chunk
mkChunk createTables ks tn lookupKeyType lookupKeyVar lookupKeyWhere =
  let (CreateTable _ks _tn cols) = findCreateTable ks tn createTables
      keySpace = ks
      tableName = tn
      keySpaceCaml = caml ks
      tableNameCaml = caml tn
      columns = T.intercalate ", " (fmap colName cols)
      columnsQuoted = T.intercalate ", " (fmap (quote . colName) cols)
      typeOfRow = T.intercalate ", " (fmap (("Maybe " <>) . toHaskellType . colType) cols)
      placeHolders = "(" <> T.intercalate ", " (replicate (length cols) "?") <> ")"
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
