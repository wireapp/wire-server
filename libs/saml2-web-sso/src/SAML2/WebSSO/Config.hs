{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
-- TODO: Create a dedicated module for orphan instances
{-# OPTIONS_GHC -Wno-orphans #-}

module SAML2.WebSSO.Config where

import Control.Exception
import Control.Lens hiding (Level, element, enum, (.=))
import Control.Monad (when)
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.ByteString (toStrict)
import Data.ByteString.Builder
import Data.Domain
import Data.Map
import Data.Schema
import Data.String.Conversions
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
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
    _cfgDomainConfigs ::
      -- if you don't use the multi-ingress feature, you only ever need one of these, so
      -- you'll use the `Left` case.
      Either MultiIngressDomainConfig (Map Domain MultiIngressDomainConfig)
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON) via Schema Config

data MultiIngressDomainConfig = MultiIngressDomainConfig
  { _cfgSPAppURI :: URI,
    _cfgSPSsoURI :: URI,
    _cfgContacts :: [ContactPerson]
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON) via Schema MultiIngressDomainConfig

-- | this looks exactly like tinylog's type, but we redefine it here to avoid the dependency.
data Level = Trace | Debug | Info | Warn | Error | Fatal
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (A.ToJSON, A.FromJSON) via Schema Level

----------------------------------------------------------------------
-- schema-profunctor

data ConfigRaw = ConfigRaw
  { _cfgRawLogLevel :: Level,
    _cfgRawSPHost :: String,
    _cfgRawSPPort :: Int,
    _cfgRawDomainConfigs :: Maybe (Map Domain MultiIngressDomainConfig),
    _cfgRawSPAppURI :: Maybe URI,
    _cfgRawSPSsoURI :: Maybe URI,
    _cfgRawContacts :: Maybe [ContactPerson]
  }

instance ToSchema ConfigRaw where
  schema =
    object "ConfigRaw" $
      ConfigRaw
        <$> (_cfgRawLogLevel .= field "logLevel" schema)
        <*> (_cfgRawSPHost .= field "spHost" schema)
        <*> (_cfgRawSPPort .= field "spPort" schema)
        <*> (_cfgRawDomainConfigs .= maybe_ (optField "spDomainConfigs" (map_ schema)))
        <*> (_cfgRawSPAppURI .= maybe_ (optField "spAppUri" schema))
        <*> (_cfgRawSPSsoURI .= maybe_ (optField "spSsoUri" schema))
        <*> (_cfgRawContacts .= maybe_ (optField "contacts" (array schema)))

instance ToSchema MultiIngressDomainConfig where
  schema =
    object "MultiIngressDomainConfig" $
      MultiIngressDomainConfig
        <$> (_cfgSPAppURI .= field "spAppUri" schema)
        <*> (_cfgSPSsoURI .= field "spSsoUri" schema)
        <*> (_cfgContacts .= field "contacts" (array schema))

instance ToSchema ContactPerson where
  schema = _ -- TODO: i think the old aeson instances are in another module, find and translate!

instance ToSchema Config where
  schema = _ -- TODO: use `withParser` on `ConfigRaw`

instance ToSchema Level where
  schema =
    enum @T.Text "Level" $
      mconcat
        [ element "Trace" Trace,
          element "Debug" Debug,
          element "Info" Info,
          element "Warn" Warn,
          element "Error" Error,
          element "Fatal" Fatal
        ]

instance ToSchema URI where
  schema = uriToText .= schema @T.Text `withParser` parseSchemaURI
    where
      uriToText :: URI -> T.Text
      uriToText = T.decodeUtf8 . toStrict . toLazyByteString . serializeURIRef

parseSchemaURI :: T.Text -> A.Parser URI
parseSchemaURI uriText =
  either
    (\e -> fail ("Failed to parse URI " ++ T.unpack uriText ++ " Error: " ++ show e))
    pure
    $ (parseURI strictURIParserOptions . T.encodeUtf8) uriText

----------------------------------------------------------------------
-- default

fallbackConfig :: Config
fallbackConfig =
  Config
    { _cfgLogLevel = Debug,
      _cfgSPHost = "localhost",
      _cfgSPPort = 8081,
      _cfgDomainConfigs =
        Left
          MultiIngressDomainConfig
            { _cfgSPAppURI = [uri|https://example-sp.com/landing|],
              _cfgSPSsoURI = [uri|https://example-sp.com/sso|],
              _cfgContacts = [fallbackContact]
            }
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
      when (_cfgLogLevel cfg <= Info)
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
      when (_cfgLogLevel cfg <= Info)
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

----------------------------------------------------------------------
-- TH stuff at the end of the module

makeLenses ''Config
