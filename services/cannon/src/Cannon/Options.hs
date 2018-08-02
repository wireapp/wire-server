{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Cannon.Options
    ( optsParser
    , host
    , port
    , cannon
    , gundeck
    , externalHost
    , externalHostFile
    , Opts
    )
where

import Control.Lens (makeFields)
import Data.Aeson.APIFieldJsonTH
import Data.Text (Text, pack)
import Data.Word
import GHC.Generics
import Options.Applicative


data Cannon = Cannon
    { _cannonHost             :: !String
    , _cannonPort             :: !Word16
    , _cannonExternalHost     :: !(Maybe Text)
    , _cannonExternalHostFile :: !(Maybe FilePath)
    } deriving (Eq, Show, Generic)

makeFields ''Cannon
deriveApiFieldJSON ''Cannon

data Gundeck = Gundeck
    { _gundeckHost  :: !Text
    , _gundeckPort  :: !Word16
    } deriving (Eq, Show, Generic)

makeFields ''Gundeck
deriveApiFieldJSON ''Gundeck

data Opts = Opts
    { _optsCannon   :: !Cannon
    , _optsGundeck  :: !Gundeck
    } deriving (Eq, Show, Generic)

makeFields ''Opts
deriveApiFieldJSON ''Opts

optsParser :: Parser Opts
optsParser = Opts <$> cParser <*> gParser

gParser :: Parser Gundeck
gParser = Gundeck
    <$> (textOption $
            long "gundeck-host"
            <> metavar "HOSTNAME"
            <> help "Gundeck host")

    <*> (option auto $
            long "gundeck-port"
            <> metavar "PORT"
            <> help "Gundeck port")


cParser :: Parser Cannon
cParser = Cannon
    <$> (strOption $
            long "host"
            <> metavar "HOSTNAME"
            <> help "host to listen on")

    <*> (option auto $
            long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "port to listen on")

    <*> (optional $ (textOption $
            long "external-host"
            <> metavar "HOSTNAME"
            <> help "host address to report to other services"))

    <*> (optional $ (strOption $
            long "external-host-file"
            <> metavar "HOSTNAME_FILE"
            <> help "file containing host address to report to other services"))


textOption :: Mod OptionFields String -> Parser Text
textOption = fmap pack . strOption
