{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- | Reading the Spar config.
module Spar.Options
  ( Opts' (..),
    Opts,
    DerivedOpts (..),
    getOpts,
    deriveOpts,
    readOptsFile,
    maxttlAuthreqDiffTime,
  )
where

import Control.Exception
import Control.Lens
import Data.Aeson hiding (fieldLabelModifier)
import qualified Data.ByteString as SBS
import Data.Time
import qualified Data.Yaml as Yaml
import Imports
import Options.Applicative
import SAML2.WebSSO
import qualified SAML2.WebSSO as SAML
import System.Logger.Extended (LogFormat)
import Text.Ascii (ascii)
import URI.ByteString
import Util.Options
import Wire.API.Routes.Version
import Wire.API.User.Orphans ()
import Wire.API.User.Saml

type Opts = Opts' DerivedOpts

data Opts' a = Opts
  { saml :: !SAML.Config,
    brig :: !Endpoint,
    galley :: !Endpoint,
    cassandra :: !CassandraOpts,
    maxttlAuthreq :: !(TTL "authreq"),
    maxttlAuthresp :: !(TTL "authresp"),
    -- | The maximum number of SCIM tokens that we will allow teams to have.
    maxScimTokens :: !Int,
    -- | The maximum size of rich info. Should be in sync with 'Brig.Types.richInfoLimit'.
    richInfoLimit :: !Int,
    -- | Wire/AWS specific; optional; used to discover Cassandra instance
    -- IPs using describe-instances.
    discoUrl :: !(Maybe Text),
    logNetStrings :: !(Maybe (Last Bool)),
    logFormat :: !(Maybe (Last LogFormat)),
    disabledAPIVersions :: !(Set VersionExp),
    derivedOpts :: !a
  }
  deriving (Functor, Show, Generic)

instance FromJSON (Opts' (Maybe ()))

data DerivedOpts = DerivedOpts
  { derivedOptsScimBaseURI :: !URI
  }
  deriving (Show, Generic)

maxttlAuthreqDiffTime :: Opts -> NominalDiffTime
maxttlAuthreqDiffTime = ttlToNominalDiffTime . maxttlAuthreq

type OptsRaw = Opts' (Maybe ())

-- | Throws an exception if no config file is found.
getOpts :: IO Opts
getOpts = do
  let desc = "Spar - SSO Service"
  deriveOpts
    =<< readOptsFile
    =<< execParser (info (helper <*> cliOptsParser) (header desc <> fullDesc))

deriveOpts :: OptsRaw -> IO Opts
deriveOpts raw = do
  derived <- do
    -- We could also make this selectable in the config file, but it seems easier to derive it from
    -- the SAML base uri.
    let derivedOptsScimBaseURI = (saml raw ^. SAML.cfgSPSsoURI) & pathL %~ derive
          where
            derive path = case reverse
              . filter (not . SBS.null)
              . SBS.split (ascii '/')
              $ path of
              ("sso" : path') -> compile path'
              path' -> compile path'
            compile path = "/" <> SBS.intercalate "/" (reverse ("v2" : "scim" : path))
    pure DerivedOpts {..}
  pure $ derived <$ raw

-- | This should not leave this module.  It is only for callling 'sparResponseURI' before the 'Spar'
-- monad is fully initialized.
newtype WithConfig a = WithConfig (Reader OptsRaw a)
  deriving (Functor, Applicative, Monad)

instance SAML.HasConfig WithConfig where
  getConfig = WithConfig $ asks saml

-- | Accept config file location as cli option.
--
-- FUTUREWORK: it would be nicer for the Parser to return the contents of the file, and return an
-- error that explains the cli options if it doesn't succeed.
cliOptsParser :: Parser FilePath
cliOptsParser =
  strOption $
    long "config-file"
      <> short 'c'
      <> help "Spar application config to load"
      <> showDefault
      <> value defaultSparPath
  where
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"

readOptsFile :: FilePath -> IO OptsRaw
readOptsFile path =
  either err1 pure =<< Yaml.decodeFileEither path
  where
    err1 = throwIO . ErrorCall . ("no or bad config file: " <>) . show
