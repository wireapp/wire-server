{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

-- FUTUREWORK: set `-XNoDeriveAnyClass`.
module SAML2.WebSSO.Config where

import Control.Exception
import Control.Lens hiding (Level, (.=))
import Control.Monad (when)
import Data.Aeson
import Data.String.Conversions
import qualified Data.Yaml as Yaml
import GHC.Generics
import SAML2.WebSSO.Types
import System.Environment
import System.FilePath
import System.IO
import URI.ByteString
import URI.ByteString.QQ

----------------------------------------------------------------------
-- data types

data Config = Config
  { _cfgLogLevel :: Level,
    _cfgSPHost :: String,
    _cfgSPPort :: Int,
    _cfgSPAppURI :: URI,
    _cfgSPSsoURI :: URI,
    _cfgContacts :: [ContactPerson]
  }
  deriving (Eq, Show, Generic)

-- | this looks exactly like tinylog's type, but we redefine it here to avoid the dependency.
data Level = Trace | Debug | Info | Warn | Error | Fatal
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, FromJSON, ToJSON)

----------------------------------------------------------------------
-- instances

makeLenses ''Config

instance ToJSON Config where
  toJSON Config {..} =
    object $
      [ "logLevel" .= _cfgLogLevel,
        "spHost" .= _cfgSPHost,
        "spPort" .= _cfgSPPort,
        "spAppUri" .= _cfgSPAppURI,
        "spSsoUri" .= _cfgSPSsoURI,
        "contacts" .= _cfgContacts
      ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> do
    _cfgLogLevel <- obj .: "logLevel"
    _cfgSPHost <- obj .: "spHost"
    _cfgSPPort <- obj .: "spPort"
    _cfgSPAppURI <- obj .: "spAppUri"
    _cfgSPSsoURI <- obj .: "spSsoUri"
    _cfgContacts <- obj .: "contacts"
    pure Config {..}

----------------------------------------------------------------------
-- default

fallbackConfig :: Config
fallbackConfig =
  Config
    { _cfgLogLevel = Debug,
      _cfgSPHost = "localhost",
      _cfgSPPort = 8081,
      _cfgSPAppURI = [uri|https://example-sp.com/landing|],
      _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
      _cfgContacts = [fallbackContact]
    }

fallbackContact :: ContactPerson
fallbackContact =
  ContactPerson
    ContactSupport
    (Just $ mkXmlText "evil corp.")
    (Just $ mkXmlText "Dr.")
    (Just $ mkXmlText "Girlfriend")
    (Just [uri|email:president@evil.corp|])
    (Just $ mkXmlText "+314159265")

----------------------------------------------------------------------
-- IO

configIO :: IO Config
configIO = readConfig =<< configFilePath

configFilePath :: IO FilePath
configFilePath = (</> "server.yaml") <$> getEnv "SAML2_WEB_SSO_ROOT" -- TODO(fisx): this doesn't work any more. think of something nicer instead of fixing it!

readConfig :: FilePath -> IO Config
readConfig filepath =
  either (\err -> fallbackConfig <$ warn err) (\cnf -> info cnf >> pure cnf)
    =<< Yaml.decodeFileEither filepath
  where
    info :: Config -> IO ()
    info cfg =
      when (cfg ^. cfgLogLevel <= Info)
        $ hPutStrLn stderr
          . ("\n>>> server config:\n" <>)
          . cs
          . Yaml.encode
        $ cfg
    warn :: Yaml.ParseException -> IO ()
    warn err =
      hPutStrLn stderr $
        "*** could not read config file: "
          <> show err
          <> "  using default!  see SAML.WebSSO.Config for details!"

-- | Convenience function to write a config file if you don't already have one.  Writes to
-- `$SAML2_WEB_SSO_ROOT/server.yaml`.  Warns if env does not contain the root.
writeConfig :: Config -> IO ()
writeConfig cfg = (`Yaml.encodeFile` cfg) =<< configFilePath

idpConfigIO :: Config -> IO [IdPConfig_]
idpConfigIO cfg = readIdPConfig cfg =<< idpConfigFilePath

idpConfigFilePath :: IO FilePath
idpConfigFilePath = (</> "idps.yaml") <$> getEnv "SAML2_WEB_SSO_ROOT"

readIdPConfig :: Config -> FilePath -> IO [IdPConfig_]
readIdPConfig cfg filepath =
  either (throwIO . ErrorCall . show) (\cnf -> info cnf >> pure cnf)
    =<< Yaml.decodeFileEither filepath
  where
    info :: [IdPConfig_] -> IO ()
    info idps =
      when (cfg ^. cfgLogLevel <= Info)
        $ hPutStrLn stderr
          . ("\n>>>known idps:\n" <>)
          . cs
          . Yaml.encode
        $ idps

----------------------------------------------------------------------
-- class

class HasConfig m where
  getConfig :: m Config

instance HasConfig ((->) Config) where
  getConfig = id
