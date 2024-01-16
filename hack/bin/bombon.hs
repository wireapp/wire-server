#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/a14de7ecd0c24192bc8d5cb534e8a0e21bbd91fa --extra-experimental-features flakes
{-# LANGUAGE DataKinds #-}

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client qualified as HTTP
import Options.Applicative
import Servant.API
import Servant.Client

data Payload = Payload
  { bom :: String,
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
  { opBomPath :: String,
    opProjectName :: String,
    opProjectVersion :: String,
    opAutoCreate :: Bool,
    opApiKey :: String
  }
  deriving (Show)

cliParser :: Parser CliOptions
cliParser =
  CliOptions
    <$> ( strOption
            ( long "bom-filepath"
                <> short 'f'
                <> metavar "FILENAME"
            )
        )
    <*> ( strOption
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
  manager' <- HTTP.newManager HTTP.defaultManagerSettings
  bom <- readFile $ opBomPath options
  let payload =
        Payload
          { bom = bom,
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
    Right res -> print res
