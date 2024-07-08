{-# LANGUAGE RecordWildCards #-}

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

module Assets.Lib where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens
import qualified Data.Attoparsec.ByteString.Char8 as Atto (Parser)
import Data.ByteString.Conversion
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import Data.Id (UserId)
import qualified Data.Text.Encoding as T
import Data.Text.Strict.Lens
import Imports
import Options.Applicative
import System.IO (hPutStr)
import qualified System.Logger as Log
import Wire.API.Asset (AssetKey)

data Opts = Opts
  { cHost :: String,
    cPort :: Int,
    cKeyspace :: C.Keyspace
  }

sampleParser :: Parser Opts
sampleParser =
  Opts
    <$> strOption
      ( long "cassandra-host"
          <> short 's'
          <> metavar "HOST"
          <> help "Cassandra Host"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "cassandra-port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Cassandra Port"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "cassandra-keyspace"
                  <> short 'k'
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace"
                  <> value "brig_test"
                  <> showDefault
              )
        )

main :: IO ()
main = do
  putStrLn "starting to read users ..."
  opts <- execParser (info (helper <*> sampleParser) desc)
  logger <- initLogger
  client <- initCas opts logger
  res <- process client
  putStrLn "\n"
  print res
  where
    initLogger = Log.new . Log.setOutput Log.StdOut . Log.setFormat Nothing . Log.setBufSize 0 $ Log.defSettings
    initCas Opts {..} l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts cHost []
        . C.setPortNumber (fromIntegral cPort)
        . C.setKeyspace cKeyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "assets" <> progDesc "find invalid asset keys in cassandra brig" <> fullDesc

selectUsersAll :: C.PrepQuery C.R () UserRow
selectUsersAll = "SELECT id, assets FROM user"

readUsers :: ClientState -> ConduitM () [UserRow] IO ()
readUsers client =
  transPipe (runClient client) $
    paginateC selectUsersAll (paramsP LocalQuorum () 500) x5

process :: ClientState -> IO Result
process client =
  runConduit $
    readUsers client
      .| Conduit.mapM (\chunk -> hPutStr stderr "." $> chunk)
      .| Conduit.concat
      .| Conduit.foldMap checkAssets
  where
    isInvalid :: AssetText -> Bool
    isInvalid asset = isLeft $ runParser (parser :: Atto.Parser AssetKey) $ T.encodeUtf8 (txtAssetKey asset)

    checkAssets :: UserRow -> Result
    checkAssets (_, Nothing) = Result 1 0 []
    checkAssets (_, Just []) = Result 1 0 []
    checkAssets row@(_, Just assets) = if any isInvalid assets then Result 0 0 [row] else Result 0 1 []

type UserRow = (UserId, Maybe [AssetText])

data Result = Result
  { noAsset :: Int,
    validAsset :: Int,
    invalidAsset :: [UserRow]
  }
  deriving stock (Eq, Generic)

newtype AssetText = ImageAssetText
  { txtAssetKey :: Text
  }
  deriving stock (Eq, Generic)

instance Show AssetText where
  show (ImageAssetText ak) = show ak

instance Cql AssetText where
  ctype =
    Tagged
      ( UdtColumn
          "asset"
          [ ("typ", IntColumn),
            ("key", TextColumn)
          ]
      )

  fromCql (CqlUdt fs) = do
    t <- required "typ"
    k <- required "key"
    case (t :: Int32) of
      0 -> pure $! ImageAssetText k
      _ -> Left $ "unexpected user asset type: " ++ show t
    where
      required :: (Cql r) => Text -> Either String r
      required f =
        maybe
          (Left ("Asset: Missing required field '" ++ show f ++ "'"))
          fromCql
          (lookup f fs)
  fromCql _ = Left "UserAsset: UDT expected"

  -- Note: Order must match up with the 'ctype' definition.
  toCql (ImageAssetText k) =
    CqlUdt
      [ ("typ", CqlInt 0),
        ("key", toCql k)
      ]

instance Show Result where
  show (Result n v i) =
    "num_no_assets: "
      <> show n
      <> "\nnum_valid_assets: "
      <> show v
      <> "\nnum_invalid_assets: "
      <> show (length i)
      <> "\ninvalid_assets:\n"
      <> concatMap showRow i
    where
      showRow (uid, Nothing) = "  - user_id: " <> show uid <> "\n"
      showRow (uid, Just as) = "  - user_id: " <> show uid <> "\n" <> showAssets as
      showAsset a = "    key: " <> show (txtAssetKey a) <> "\n"
      showAssets assets = concatMap showAsset assets

instance Semigroup Result where
  (<>) (Result n1 v1 i1) (Result n2 v2 i2) =
    Result (n1 + n2) (v1 + v2) (i1 <> i2)

instance Monoid Result where
  mempty = Result 0 0 []
  mappend = (<>)
