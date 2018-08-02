{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.Options
  ( getOpts
  , deriveOpts
  , readOptsFile
  ) where

import Control.Exception
<<<<<<< HEAD
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Monoid
=======
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import GHC.Types (Symbol)
import Lens.Micro
>>>>>>> bcca3a25... Imports (you can skip this one)
import Options.Applicative
import Spar.API.Types
import Spar.Types
import URI.ByteString as URI

import qualified Data.Yaml as Yaml
import qualified SAML2.WebSSO as SAML


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
        unwrap = maybe (throwM $ ErrorCall "Bad server config: no domain in response URI") pure
    derivedOptsBindCookieDomain <- URI.hostBS . URI.authorityHost <$> unwrap (URI.uriAuthority respuri)
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
cliOptsParser = strOption $
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
