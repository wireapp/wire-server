{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Spar.Options
  ( Opts(..)
  , TTL(..), TTLError(..)
  , getOpts
  , readOptsFile
  , ttlToNominalDiffTime
  , maxttlAuthreqDiffTime
  ) where

import Control.Exception
import Data.Aeson
import Data.Int
import Data.Monoid
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import GHC.Types (Symbol)
import Lens.Micro
import Options.Applicative
import Util.Options hiding (getOptions)
import Spar.Types (IdPExtra, SPInfo)

import qualified Data.Yaml as Yaml
import qualified SAML2.WebSSO.Config as SAML


data Opts = Opts
    { saml           :: !(SAML.Config IdPExtra)
    , spInfo         :: !SPInfo
    , brig           :: !Endpoint
    , cassandra      :: !CassandraOpts
    , maxttlAuthreq  :: !(TTL "authreq")
    , maxttlAuthresp :: !(TTL "authresp")
    , discoUrl       :: !(Maybe Text) -- Wire/AWS specific; optional; used to discover cassandra instance IPs using describe-instances
    , logNetStrings  :: !Bool
    -- , optSettings   :: !Settings  -- (nothing yet; see other services for what belongs in here.)
    }
  deriving (Show, Generic)

instance FromJSON Opts

-- | (seconds)
newtype TTL (tablename :: Symbol) = TTL { fromTTL :: Int32 }
  deriving (Eq, Ord, Show, Num)

instance FromJSON (TTL a) where
  parseJSON = withScientific "TTL value (seconds)" (pure . TTL . round)

data TTLError = TTLTooLong | TTLNegative
  deriving (Eq, Show)

ttlToNominalDiffTime :: TTL a -> NominalDiffTime
ttlToNominalDiffTime (TTL i32) = fromIntegral i32

maxttlAuthreqDiffTime :: Opts -> NominalDiffTime
maxttlAuthreqDiffTime = ttlToNominalDiffTime . maxttlAuthreq


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

readOptsFile :: FilePath -> IO Opts
readOptsFile path =
  either err1 (\opts -> if hasNoIdPs opts then pure opts else err2)
    =<< Yaml.decodeFileEither path
  where
    err1 = throwIO . ErrorCall . ("no or bad config file: " <>) . show
    err2 = throwIO $ ErrorCall "idps field is not supported by spar."

    hasNoIdPs :: Opts -> Bool
    hasNoIdPs = null . (^. to saml . SAML.cfgIdps)
