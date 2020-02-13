{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cannon.Options
  ( host,
    port,
    cannon,
    gundeck,
    externalHost,
    externalHostFile,
    logLevel,
    logNetStrings,
    logFormat,
    Opts,
  )
where

import Control.Lens (makeFields)
import Data.Aeson.APIFieldJsonTH
import Imports
import System.Logger.Extended (Level, LogFormat)

data Cannon
  = Cannon
      { _cannonHost :: !String,
        _cannonPort :: !Word16,
        _cannonExternalHost :: !(Maybe Text),
        _cannonExternalHostFile :: !(Maybe FilePath)
      }
  deriving (Eq, Show, Generic)

makeFields ''Cannon

deriveApiFieldJSON ''Cannon

data Gundeck
  = Gundeck
      { _gundeckHost :: !Text,
        _gundeckPort :: !Word16
      }
  deriving (Eq, Show, Generic)

makeFields ''Gundeck

deriveApiFieldJSON ''Gundeck

data Opts
  = Opts
      { _optsCannon :: !Cannon,
        _optsGundeck :: !Gundeck,
        _optsLogLevel :: !Level,
        _optsLogNetStrings :: !(Maybe (Last Bool)),
        _optsLogFormat :: !(Maybe (Last LogFormat))
      }
  deriving (Eq, Show, Generic)

makeFields ''Opts

deriveApiFieldJSON ''Opts
