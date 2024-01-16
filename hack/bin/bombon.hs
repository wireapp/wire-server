#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/99fe5a331fdd37d52043f14e5c565ac29a30bcb4
{-# LANGUAGE DataKinds #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as Base64
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.API
import Servant.Client
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

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
  manager' <- HTTP.newManager tlsManagerSettings
  bom <- readFile $ opBomPath options
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
    Right res -> print res

toBase64Text :: String -> Text
toBase64Text = decodeUtf8 . Base64.encode . BL.pack
