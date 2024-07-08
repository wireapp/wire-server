#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/74d6dd639d1da35a8d361e8cd2274b1cfbe8381c
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Aeson
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.ByteString.Lazy
import Data.Proxy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Sbom hiding (main)
import Servant.API
import Servant.Client
import System.Exit
import System.Process

data Payload = Payload
  { bom :: Text,
    projectName :: String,
    projectVersion :: String,
    autoCreate :: Bool
  }
  deriving (Generic, Show)

instance ToJSON Payload

data ApiResponse = ApiResponse
  { token :: String
  }
  deriving (Generic, Show)

instance FromJSON ApiResponse

type DependenyTrackAPI =
  "api"
    :> "v1"
    :> "bom"
    :> ReqBody '[JSON] Payload
    :> Header "X-Api-Key" String
    :> Put '[JSON] ApiResponse

api :: Proxy DependenyTrackAPI
api = Proxy

putBOM :: Payload -> Maybe String -> ClientM ApiResponse
putBOM = client api

data CliOptions = CliOptions
  { opProjectName :: String,
    opProjectVersion :: String,
    opAutoCreate :: Bool,
    opApiKey :: String
  }
  deriving (Show)

cliParser :: Parser CliOptions
cliParser =
  CliOptions
    <$> ( strOption
            ( long "project-name"
                <> short 'p'
                <> metavar "PROJECT_NAME"
                <> value "wire-server-ci"
            )
        )
    <*> ( strOption
            ( long "project-version"
                <> short 'v'
                <> metavar "PROJECT_VERSION"
            )
        )
    <*> ( switch
            ( long "auto-create"
                <> short 'c'
            )
        )
    <*> ( strOption
            ( long "api-key"
                <> short 'k'
                <> metavar "API_KEY"
            )
        )

fullCliParser :: ParserInfo CliOptions
fullCliParser =
  info
    (cliParser <**> helper)
    ( fullDesc
        <> progDesc "Upload BOM files to deptrack"
    )

main :: IO ()
main = do
  options <- execParser fullCliParser
  manager' <- HTTP.newManager tlsManagerSettings
  buildWire <- spawnCommand "nix -Lv build -f ../../nix wireServer.allLocalPackages -o wire-server"
  buildMeta <- spawnCommand "nix -Lv build -f ../../nix wireServer.toplevel-derivations --impure -o meta"
  waitForProcess buildWire >>= \case
    ExitFailure _ -> fail "process for building wire failed"
    ExitSuccess -> putStrLn "finished building Wire"
  waitForProcess buildMeta >>= \case
    ExitFailure _ -> fail "process for building meta for wire failed"
    ExitSuccess -> putStrLn "finished building meta"

  bom <- mainNoParse ("./meta", "./wire-server")
  let payload =
        Payload
          { bom = toBase64Text bom,
            projectName = opProjectName options,
            projectVersion = opProjectVersion options,
            autoCreate = opAutoCreate options
          }
  res <-
    runClientM
      (putBOM payload ((Just . opApiKey) options))
      (mkClientEnv manager' (BaseUrl Https "deptrack.wire.link" 443 ""))
  case res of
    Left err -> print $ "Error: " ++ show err
    Right res' -> print res'

toBase64Text :: LazyByteString -> Text
toBase64Text = decodeUtf8 . Base64.encode
