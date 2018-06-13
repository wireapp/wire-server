{-# LANGUAGE DeriveGeneric #-}
module Spar.Options
  ( Opts(..)
  , getOpts
  , readOptsFile
  ) where

import Control.Exception
import Data.Aeson
import Data.Monoid
import Data.Id
import GHC.Generics
import Util.Options hiding (getOptions)
import Options.Applicative
import qualified Data.Yaml as Yaml

import qualified SAML2.WebSSO.Config as SAML2


data Opts = Opts
    { saml          :: !(SAML2.Config TeamId)
    , brig          :: !Endpoint
    , cassandra     :: !CassandraOpts
    -- , optSettings   :: !Settings  -- (nothing yet; see other services for what belongs in here.)
    }
  deriving (Show, Generic)

instance FromJSON Opts

-- | Throws an exception if no config file is found.
getOpts :: IO Opts
getOpts = do
  let desc = "Spar - SSO Service"
  readOptsFile =<< execParser (info (helper <*> cliOptsParser) (header desc <> fullDesc))

-- | Accept config file location as cli option.
--
-- FUTUREWORK: it would be nicer for the Parser to return the contents of the file, and return an
-- error that explains the cli options if it doesn't succeed.
cliOptsParser :: Parser FilePath
cliOptsParser = strOption $
    long "config-file"
    <> short 'c'
    <> help "Spar application config to load"
    <> showDefault
    <> value defaultSparPath
  where
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"

readOptsFile :: (FromJSON a) => FilePath -> IO a
readOptsFile path =
  either (throwIO . ErrorCall . ("no or bad config file: " <>) . show) pure
    =<< Yaml.decodeFileEither path
