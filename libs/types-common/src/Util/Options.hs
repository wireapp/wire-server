module Util.Options where

import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Yaml (ParseException, decodeFileEither)
import Options.Applicative
import System.Directory
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

getOptions :: (FromJSON a) => String -> Parser a -> FilePath -> IO a
getOptions desc parser defaultPath = do
    path <- parseConfigPath defaultPath (mkDesc desc)
    file <- doesFileExist path
    if file
        then do
            configFile <- decodeConfigFile path
            case configFile of
                Left e -> fail $ show e
                Right opts -> return opts
        else do
            hPutStrLn stderr $
                "Config file at " ++
                path ++
                " does not exist, falling back to command-line arguments. \n"
            execParser (info (helper <*> parser) (mkDesc desc))

decodeConfigFile :: (FromJSON a) => FilePath -> IO (Either ParseException a)
decodeConfigFile = decodeFileEither

parseConfigPath :: FilePath -> InfoMod String -> IO String
parseConfigPath defaultPath desc = do
    args <- getArgs
    let result =
            getParseResult $
            execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
    pure $ fromMaybe defaultPath result
  where
    pathParser :: Parser String
    pathParser =
        strOption $
        long "config-file" <> short 'c' <> help "Config file to load" <>
        showDefault <>
        value defaultPath

mkDesc :: String -> InfoMod a
mkDesc desc = header desc <> fullDesc
