{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Proxy.Options
    ( Opts
    , host
    , port
    , secretsConfig
    , httpPoolSize
    , maxConns
    , optsParser
    ) where

import Control.Lens
import Data.Monoid
import Data.Word
import Options.Applicative
import Data.Aeson
import Data.Aeson.TH
import GHC.Generics

data Opts = Opts
    { _host          :: !String
    , _port          :: !Word16
    , _secretsConfig :: !FilePath
    , _httpPoolSize  :: !Int
    , _maxConns      :: !Int
    } deriving (Show, Generic)

makeLenses ''Opts

deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Opts

optsParser :: Parser Opts
optsParser = Opts
    <$> (strOption $
            long "host"
            <> value "*4"
            <> showDefault
            <> metavar "HOSTNAME"
            <> help "host to listen on")

    <*> (option auto $
            long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "listen port")

    <*> (strOption $
            long "config"
            <> metavar "FILE"
            <> help "File containing upstream secrets"
            <> action "file")

    <*> (option auto $
            long "http-pool-size"
            <> metavar "SIZE"
            <> showDefault
            <> help "number of connections for the http pool"
            <> value 256)

    <*> (option auto $
            long "max-connections"
            <> metavar "SIZE"
            <> help "maximum number of incoming connections")
