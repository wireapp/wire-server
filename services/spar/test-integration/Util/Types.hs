{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

module Util.Types
  ( BrigReq
  , GalleyReq
  , SparReq
  , TestEnv(..)
  , teMgr
  , teCql
  , teBrig
  , teGalley
  , teSpar
  , teNewIdP
  , teUserId
  , teTeamId
  , teIdP
  , teIdPChan
  , teIdPHandle
  , teOpts
  , teTstOpts
  , Select
  , ResponseLBS
  , IntegrationConfig(..)
  , MockIdPConfig(..)
  , TestErrorLabel(..)
  ) where

import Bilge
import Cassandra as Cas
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Id
import Data.String
import Data.String.Conversions
import GHC.Generics (Generic)
import Lens.Micro.TH
import SAML2.WebSSO as SAML
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Spar.API ()
import Spar.Options as Options
import Spar.Types
import Text.XML
import URI.ByteString
import Util.Options

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.X509 as X509


type BrigReq   = Request -> Request
type GalleyReq = Request -> Request
type SparReq   = Request -> Request

-- | See 'mkEnv' about what's in here.
data TestEnv = TestEnv
  { _teMgr         :: Manager
  , _teCql         :: Cas.ClientState
  , _teBrig        :: BrigReq
  , _teGalley      :: GalleyReq
  , _teSpar        :: SparReq
  , _teNewIdP      :: SAML.NewIdP        -- ^ used for registering the mock idp with spar
  , _teUserId      :: UserId             -- ^ owner of the mock idp's home team
  , _teTeamId      :: TeamId             -- ^ home team of the mock idp
  , _teIdP         :: IdP                -- ^ mock idp details
  , _teIdPChan     :: TChan [Node]       -- ^ channel for feeding (broken) metadata values for testing error handling
  , _teIdPHandle   :: Async.Async ()     -- ^ mock idp thread handle
  , _teOpts        :: Opts               -- ^ spar config
  , _teTstOpts     :: IntegrationConfig  -- ^ integration test config
  }

type Select = TestEnv -> (Request -> Request)

type ResponseLBS = Bilge.Response (Maybe LBS)

data IntegrationConfig = IntegrationConfig
  { cfgBrig    :: Endpoint
  , cfgGalley  :: Endpoint
  , cfgSpar    :: Endpoint
  , cfgMockIdp :: MockIdPConfig
  } deriving (Show, Generic)

data MockIdPConfig = MockIdPConfig
  { mockidpBind        :: Endpoint
  , mockidpConnect     :: Endpoint
  , mockidpPrivateKey  :: FilePath
  , mockidpPublicKey   :: FilePath
  , mockidpCert        :: FilePath
  , mockidpMetadataURI :: URI
  , mockidpIssuer      :: Issuer
  , mockidpRequestURI  :: URI
  , mockidpDSigCert    :: X509.SignedCertificate
  } deriving (Show, Generic)

deriveFromJSON deriveJSONOptions ''IntegrationConfig
deriveFromJSON deriveJSONOptions ''MockIdPConfig
makeLenses ''TestEnv


newtype TestErrorLabel = TestErrorLabel { fromTestErrorLabel :: ST }
  deriving (Eq, Show, IsString)

instance FromJSON TestErrorLabel where
  parseJSON = fmap TestErrorLabel . withObject "TestErrorLabel" (.: "label")


-- A quick unit test that serves two purposes: (1) shows that it works (and helped with debugging);
-- (2) demonstrates how to use it.
_unitTestTestErrorLabel :: IO ()
_unitTestTestErrorLabel = do
  let val :: Either String TestErrorLabel
      val = Aeson.eitherDecode "{\"code\":404,\"message\":\"Not found.\",\"label\":\"not-found\"}"
  unless (val == Right "not-found") $
    throwIO . ErrorCall . show $ val
