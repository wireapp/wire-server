module Main where

import Imports
import Control.Lens
import Control.Monad.Except
import Data.Binary.Builder (toLazyByteString)
import Data.List (nub, sort)
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Network.DNS
import URI.ByteString
import WithCli

import qualified Data.Text.IO as T
import qualified Data.Text as T


-- * data

data Command
  = Urls
  | Domains
  | HostsFile
  | DnsmasqConf
  | WarmupScript
  | ZoneFile
  deriving (Eq, Show, Bounded, Enum, Generic)

data File = File FilePath
  deriving (Eq, Show, Typeable, Generic)

data Entry = Entry
  { _entrySize :: Int
  , _entryVerb :: Text
  , _entryURI  :: URI
  }
  deriving (Eq, Show, Generic)

makeLenses ''Entry


instance HasArguments Command where
  argumentsParser = atomicArgumentsParser

instance Argument Command where
  argumentType Proxy = show @[Command] [minBound..]
  parseArgument x = case [ x' | x' <- [minBound..], show x' == x ] of
    [x'] -> Just x'
    bad  -> error $ "invalid command: " <> show bad

instance HasArguments File where
  argumentsParser = atomicArgumentsParser

instance Argument File where
  argumentType Proxy = "file"
  parseArgument f = Just (File f)


data ParseError = ParseError
    Int   -- ^ line number
    Text  -- ^ log line
    Text  -- ^ error msg
  deriving (Eq, Show)

die :: MonadError ParseError m => Int -> Text -> Text -> m a
die l t e = throwError $ ParseError l t e

readLogLines :: Text -> ([ParseError], [Entry])
readLogLines = Imports.foldl' sieve ([], []) . zipWith readLogLine [1..] . T.lines
  where
    sieve :: ([a], [b]) -> Either a b -> ([a], [b])
    sieve (as, bs) (Left  a) = (a : as, bs)
    sieve (as, bs) (Right b) = (as,     b : bs)

readLogLine :: HasCallStack => Int -> Text -> Either ParseError Entry
readLogLine line txt = do
  case T.words txt of
    [_, _, _, _, size, verb, uri, _, _, _] -> do
      case readURI uri of
        Left err  -> die line txt (cs $ show err)
        Right val -> pure $ Entry (read $ cs size) verb val
    bad -> die line txt . cs $ "wrong number of columns: " <> show bad

readURI :: Text -> Either URIParseError URI
readURI txt = parseURI laxURIParserOptions . cs . fixSquidLogs $ txt
  where
    -- the access logs we are parsing here sometimes have an IP address as a domain and no
    -- scheme.  this is invalid according to uri-bytestring, so we add default scheme http
    -- if none is set.
    fixSquidLogs :: Text -> Text
    fixSquidLogs s = case T.splitOn "://" s of
      [_, _] -> s
      [_]    -> "http://" <> s
      bad    -> error $ "instance FromJSON URI: " <> show (s, bad)


-- * main

main :: IO ()
main = withCli run

run :: Command -> File -> IO ()
run cmd (File input) = do
  entries :: [Entry] <- do
    mentries <- readLogLines <$> T.readFile input
    case mentries of
      ([], es) -> pure es
      (errs, _) -> error . unlines $ show <$> errs

  putStrLn . cs =<< case cmd of
    Urls         -> pure $ cmdUrls entries
    Domains      -> pure $ cmdDomains entries
    HostsFile    -> cmdHostsFile entries
    DnsmasqConf  -> cmdDnsmasqConf entries
    WarmupScript -> pure $ cmdWarmupScript entries
    ZoneFile     -> pure $ cmdZoneFile entries


-- * commands

cmdUrls :: [Entry] -> Text
cmdUrls
  = T.intercalate "\n"
  . nub . sort
  . fmap (cs . toLazyByteString . serializeURIRef . view entryURI)


cmdDomains :: [Entry] -> Text
cmdDomains
  = T.intercalate "\n" . getDomains

getDomains :: [Entry] -> [Text]
getDomains
  = nub . sort
  . fmap (cs . hostBS)
  . catMaybes
  . fmap (^? entryURI . authorityL . _Just . authorityHostL)

cmdHostsFile :: [Entry] -> IO Text
cmdHostsFile entries =
  T.intercalate "\n" . fmap (\(ip, host) -> cs $ ip <> " " <> host) <$>
    getIps entries

cmdDnsmasqConf :: [Entry] -> IO Text
cmdDnsmasqConf entries =
  T.intercalate "\n" . fmap (\(ip, host) -> cs $ "address=/" <> host <> "/" <> ip) <$>
    getIps entries

getIps :: [Entry] -> IO [(Text, Text)]
getIps entries = do
  seed <- makeResolvSeed defaultResolvConf
  withResolver seed $ \resolver -> join <$> do
    forM (getDomains entries) $ \domainName -> do
      resp <- lookupA resolver (cs domainName)
      case resp of
        Left _err -> trace (show (domainName, resp)) $ pure []
        Right ips -> pure $ (, cs domainName) . cs . show <$> ips

cmdWarmupScript :: [Entry] -> Text
cmdWarmupScript = T.intercalate "\n" . ("#!/bin/bash\nset -xe" :) . nub . sort . fmap mkcurl
  where
    mkcurl entry = "curl -s " <>
      (cs . toLazyByteString . serializeURIRef $ entry ^. entryURI) <>
      " >/dev/null"

cmdZoneFile :: [Entry] -> Text
cmdZoneFile = undefined
