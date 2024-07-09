{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module ParseSchema where

import Data.Aeson (ToJSON, object, (.=))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TIO
import Imports
import Options.Applicative qualified as OA
import System.Environment (withArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.FilePath.Posix (takeDirectory)
import System.IO (Handle)
import System.Process (system)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
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

-- FUTUREWORK: the special cases in the first 6 lines should be handled by the parser, and this
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
toHaskellType "asset" = "AssetIgnoreData" -- FUTUREWORK: check if we really dont need data of this type
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
    <*> optional
      ( OA.strOption
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
import Galley.Cassandra.Instances ()
import Imports
import Types
import Wire.API.Team.Permission
import Data.IP (IP)
import Data.Handle (Handle)
import Wire.API.User.Password (PasswordResetKey)
import System.FilePath.Posix ((</>))

-- This file was autogenerated by move-team-generate


{{#chunkTable}}

-- {{keySpace}}.{{tableName}}

type Row{{keySpaceCaml}}{{tableNameCaml}} = ({{{typeOfRow}}})

select{{keySpaceCaml}}{{tableNameCaml}} :: PrepQuery R (Identity ({{lookupKeyType}})) Row{{keySpaceCaml}}{{tableNameCaml}}
select{{keySpaceCaml}}{{tableNameCaml}} = "SELECT {{columns}} FROM {{tableName}} WHERE {{lookupKeyWhere}}"

read{{keySpaceCaml}}{{tableNameCaml}}:: Env -> {{lookupKeyType}} -> ConduitM () [Row{{keySpaceCaml}}{{tableNameCaml}}] IO ()
read{{keySpaceCaml}}{{tableNameCaml}} Env {..} {{lookupKeyVar}} =
  transPipe (runClient env{{keySpaceCaml}}) $
    paginateC select{{keySpaceCaml}}{{tableNameCaml}} (paramsP LocalQuorum (pure {{lookupKeyVar}}) envPageSize) x5

select{{keySpaceCaml}}{{tableNameCaml}}All :: PrepQuery R () Row{{keySpaceCaml}}{{tableNameCaml}}
select{{keySpaceCaml}}{{tableNameCaml}}All = "SELECT {{columns}} FROM {{tableName}}"

read{{keySpaceCaml}}{{tableNameCaml}}All :: Env -> ConduitM () [Row{{keySpaceCaml}}{{tableNameCaml}}] IO ()
read{{keySpaceCaml}}{{tableNameCaml}}All Env {..} =
  transPipe (runClient env{{keySpaceCaml}}) $
    paginateC select{{keySpaceCaml}}{{tableNameCaml}}All (paramsP LocalQuorum () envPageSize) x5

export{{keySpaceCaml}}{{tableNameCaml}}Full :: Env -> FilePath -> IO ()
export{{keySpaceCaml}}{{tableNameCaml}}Full env@Env {..} path = do
  putStrLn $ "Exporting " <> "{{keySpace}}.{{tableName}}"  <> " to " <> path
  withBinaryFile path WriteMode $ \handle ->
    runConduit $
      read{{keySpaceCaml}}{{tableNameCaml}}All env
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
                  .= [ -- brig.activation_keys,
                       -- brig.blacklist
                       -- brig.budget
                       -- brig.clients
                       --   PRIMARY KEY (user, client)
                       mkChunkUsers "brig" "clients",
                       -- brig.codes
                       -- brig.connection
                       --   PRIMARY KEY (left, right)
                       --   FUTUREWORK: is this symmetric? At first glance it looks like yes
                       mkChunk' "brig" "connection" "[UserId]" "uids" "left in ?",
                       -- brig.excluded_phones
                       --   PRIMARY KEY (mapped_id)
                       --   FUTUREWORK: is mapped_id a user id?
                       -- brig.invitation
                       --   mkChunk' "brig" "invitation" "[UserId]" "uids" "inviter in ?",
                       -- brig.invitation_info
                       --   mkChunk' "brig" "invitation_info" "[UserId]" "uids" "inviter in ?",
                       --   mkChunk createTables "brig" "invitee_info" "[UserId]" "uids" "invitee in ? OR inviter in ?" "([UserId], [UserId])" "(uids, uids)",
                       -- brig.login_codes
                       --   PRIMARY KEY (user)
                       --   FUTUREWORK: do we need this?
                       mkChunkUsers "brig" "login_codes",
                       -- brig.meta
                       -- brig.password_reset
                       --   PRIMARY KEY(key)
                       --   FUTUREWORK: do we need this? can we do better than a full table scan?
                       mkChunk' "brig" "password_reset" "[PasswordResetKey]" "reset_keys" "key in ?",
                       -- brig.prekeys
                       --   PRIMARY KEY (user, client, key)
                       --   FUTUREWORK: do new need this ?
                       mkChunkUsers "brig" "prekeys",
                       -- brig.properties
                       --   PRIMARY KEY (user, key)
                       mkChunkUsers "brig" "properties",
                       -- brig.provider
                       -- brig.provider_keys,
                       -- brig.rich_info
                       --   PRIMARY KEY (user)
                       mkChunkUsers "brig" "rich_info",
                       -- brig.service
                       -- brig.service_prefix
                       -- brig.service_tag
                       -- brig.service_team
                       -- brig.service_user
                       -- brig.service_whitelist
                       -- brig.service_whitelist_rev
                       -- brig.team_invitation
                       -- brig.team_invitation_email
                       -- brig.team_invitation_info
                       -- brig.unique_claims
                       -- brig.user
                       --   PRIMARY KEY (id)
                       mkChunk' "brig" "user" "[UserId]" "uids" "id in ?",
                       -- brig.user_cookies
                       -- brig.user_handle
                       --   PRIMARY KEY (handle :: text)
                       --   FUTUREWORK: can we do better than a full table scan?
                       mkChunk' "brig" "user_handle" "[Handle]" "handles" "handle in ?",
                       -- brig.user_keys:
                       --   PRIMARY KEY (key :: text) (Brig.Data.UserKey)
                       --   FUTUREWORK: do we need it?  can we do better than a full table scan?
                       mkChunk' "brig" "user_keys" "[Int32]" "keys" "key in ?",
                       -- brig.vcodes
                       -- galley.billing_team_member
                       --   PRIMARY KEY (team, user)
                       mkChunkTeam "galley" "billing_team_member",
                       -- galley.clients
                       --   PRIMARY KEY (user)
                       mkChunkUsers "galley" "clients",
                       -- galley.conversation
                       --   PRIMARY KEY (conv)
                       mkChunk' "galley" "conversation" "[ConvId]" "cids" "conv in ?",
                       -- galley.conversation_codes
                       -- galley.custom_backend
                       -- galley.data_migration
                       -- galley.id_mapping
                       -- galley.legalhold_pending_prekeys
                       -- galley.legalhold_pending_prekeys,
                       -- galley.legalhold_service,
                       -- galley.member
                       --   PRIMARY KEY (conv, user)
                       mkChunk' "galley" "member" "[ConvId]" "cids" "conv in ?",
                       -- galley.meta
                       -- galley.service
                       -- galley.team
                       --   PRIMARY KEY (team)
                       mkChunkTeam "galley" "team",
                       -- galley.team_conv
                       --   PRIMARY KEY (team, conv)
                       mkChunkTeam "galley" "team_conv",
                       -- galley.team_features
                       --   PRIMARY KEY (team_id)
                       mkChunk' "galley" "team_features" "TeamId" "tid" "team_id = ?",
                       -- galley.team_member
                       --   PRIMARY KEY (team, user)
                       mkChunkTeam "galley" "team_member",
                       -- galley.team_notifications
                       --   PRIMARY KEY (team, id)
                       mkChunkTeam "galley" "team_notifications",
                       -- galley.user
                       --   PRIMARY KEY (user, conv)
                       mkChunkUsers "galley" "user",
                       -- galley.user_team
                       --   PRIMARY KEY (user, team)
                       mkChunkUsers "galley" "user_team",
                       -- gundeck.met
                       --   PRIMARY KEY (user, id)
                       mkChunkUsers "gundeck" "notifications",
                       -- gundeck.push
                       -- gundeck.user_push
                       -- spar.authreq
                       -- spar.authresp
                       -- spar.bind_cookie
                       -- spar.default_idp
                       -- spar.idp
                       -- spar.idp_raw_metadata
                       -- spar.issuer_idp
                       -- spar.meta
                       -- spar.scim_external
                       mkChunkTeam "spar" "scim_external",
                       -- spar.scim_user_times
                       --   PRIMARY KEY (uid)
                       mkChunk' "spar" "scim_user_times" "[UserId]" "uids" "uid in ?",
                       -- spar.team_idp
                       -- spar.team_provisioning_by_team
                       -- spar.team_provisioning_by_token
                       -- spar.user
                       mkChunk' "spar" "user" "[Text]" "issuer" "issuer in ?"
                       -- PRIMARY KEY (issuer, sso_id)
                       -- spar.verdict
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
  cassandraSchema <- projectFile "cassandra-schema.cql"
  withArgs [cassandraSchema] main

debugwrite :: IO ()
debugwrite = do
  cassandraSchema <- projectFile "cassandra-schema.cql"
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
