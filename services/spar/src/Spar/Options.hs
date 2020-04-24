{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
--
-- The config type itself, 'Opts', is defined in "Spar.Types".
module Spar.Options
  ( getOpts,
    deriveOpts,
    readOptsFile,
  )
where

import Control.Exception
import Control.Lens
import qualified Data.ByteString as SBS
import qualified Data.Yaml as Yaml
import Imports
import Options.Applicative
import qualified SAML2.WebSSO as SAML
import Spar.API.Types
import Spar.Types
import Text.Ascii (ascii)
import URI.ByteString as URI

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
    let respuri = runWithConfig raw sparResponseURI
        derivedOptsBindCookiePath = URI.uriPath respuri
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

runWithConfig :: OptsRaw -> WithConfig a -> a
runWithConfig opts (WithConfig act) = act `runReader` opts

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
