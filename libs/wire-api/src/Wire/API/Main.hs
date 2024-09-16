module Wire.API.Main where

import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Text.Lazy.IO qualified as LText
import Imports
import Options.Applicative
import Wire.API.Routes.Version
import Wire.API.Swagger

data GenSwaggerOptions = GenSwaggerOptions
  { version :: Version
  }

main :: IO ()
main = do
  opts <- execParser $ info (optionsParser <**> helper) fullDesc
  LText.putStrLn . Aeson.encodeToLazyText $ case opts.version of
    V0 -> publicAPISwagger @V0
    V1 -> publicAPISwagger @V1
    V2 -> publicAPISwagger @V2
    V3 -> publicAPISwagger @V3
    V4 -> publicAPISwagger @V4
    V5 -> publicAPISwagger @V5
    V6 -> publicAPISwagger @V6
    V7 -> publicAPISwagger @V7

optionsParser :: Parser GenSwaggerOptions
optionsParser =
  subparser
    ( command
        "generate-swagger"
        (info (genSweaggerParser <**> helper) (progDesc "Generate swagger.json"))
    )
  where
    genSweaggerParser :: Parser GenSwaggerOptions
    genSweaggerParser =
      GenSwaggerOptions
        <$> option
          (maybeReader versionReader)
          ( long "version"
              <> metavar "WIRE_API_VERSION"
              <> value maxBound
              <> help "version number for which swagger should be generated"
              <> showDefaultWith (show . versionInt @Int)
          )

    versionReader :: String -> Maybe Version
    versionReader =
      fmap fromVersionNumber
        . Aeson.decode
        . BS.fromStrict
        . UTF8.fromString
