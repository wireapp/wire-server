{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Util.Types where

import Bilge
import Cassandra as Cas
import Data.Aeson.TH
import Data.String.Conversions
import GHC.Generics (Generic)
import Lens.Micro.TH
import SAML2.WebSSO.Config.TH (deriveJSONOptions)
import Spar.API ()
import Spar.Options as Options
import Spar.Types
import Util.Options


type BrigReq   = Request -> Request
type GalleyReq = Request -> Request
type SparReq   = Request -> Request

data TestEnv = TestEnv
  { _teMgr     :: Manager
  , _teCql     :: Cas.ClientState
  , _teBrig    :: BrigReq
  , _teGalley  :: GalleyReq
  , _teSpar    :: SparReq
  , _teNewIdp  :: NewIdP
  , _teMockIdp :: Endpoint
  , _teOpts    :: Opts
  }

type Select = TestEnv -> (Request -> Request)

data IntegrationConfig = IntegrationConfig
  { cfgBrig    :: Endpoint
  , cfgGalley  :: Endpoint
  , cfgSpar    :: Endpoint
  , cfgNewIdp  :: NewIdP
  , cfgMockIdp :: Endpoint
  } deriving (Show, Generic)

type ResponseLBS = Response (Maybe LBS)

deriveFromJSON deriveJSONOptions ''IntegrationConfig
makeLenses ''TestEnv
