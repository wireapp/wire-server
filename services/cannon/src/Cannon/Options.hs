{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cannon.Options
    ( host
    , port
    , cannon
    , gundeck
    , externalHost
    , externalHostFile
    , kubernetesServiceAddress
    , logLevel
    , logNetStrings
    , Opts
    , hostnameEnvKey
    )
where

import Imports
import Control.Lens (makeFields)
import Data.Aeson.APIFieldJsonTH
import System.Logger (Level)

-- | The key to check for the current cannon's hostname
-- This CANNOT be changed, as it's set by kubernetes itself when using stateful
-- sets.
hostnameEnvKey :: String
hostnameEnvKey = "HOSTNAME"

data Cannon = Cannon
    { _cannonHost                       :: !String
    , _cannonPort                       :: !Word16
    , _cannonExternalHost               :: !(Maybe Text)
    , _cannonExternalHostFile           :: !(Maybe FilePath)
    , _cannonKubernetesServiceAddress   :: !(Maybe Text)
    -- ^ Address of the kubernetes service which routes to cannons if running on k8s
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
    { _optsCannon         :: !Cannon
    , _optsGundeck        :: !Gundeck
    , _optsLogLevel       :: !Level
    , _optsLogNetStrings  :: !Bool
    } deriving (Eq, Show, Generic)

makeFields ''Opts
deriveApiFieldJSON ''Opts
