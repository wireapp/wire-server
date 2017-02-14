{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Proxy.Options
    ( Opts
    , parseOptions
    , hostname
    , port
    , config
    , httpPoolSize
    , maxConns
    ) where

import Control.Lens
import Data.Monoid
import Data.Word
import Options.Applicative

data Opts = Opts
    { _hostname     :: !String
    , _port         :: !Word16
    , _config       :: !FilePath
    , _httpPoolSize :: !Int
    , _maxConns     :: !Int
    }

makeLenses ''Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Proxy - 3rd party proxy" <> fullDesc

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
