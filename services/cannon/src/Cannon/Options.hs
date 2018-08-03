{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Cannon.Options
    ( host
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
import Data.Text (Text)
import Data.Word
import GHC.Generics


data Cannon = Cannon
    { _cannonHost             :: !String            -- ^ Host to listen on
    , _cannonPort             :: !Word16            -- ^ Port to listen on
    , _cannonExternalHost     :: !(Maybe Text)      -- ^ Host to report to other services
    , _cannonExternalHostFile :: !(Maybe FilePath)  -- ^ File containing host address to
                                                    --   report to other services
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
    { _optsCannon   :: !Cannon   -- ^ Our options
    , _optsGundeck  :: !Gundeck  -- ^ Gundeck endpoint
    } deriving (Eq, Show, Generic)

makeFields ''Opts
deriveApiFieldJSON ''Opts
