{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Types
  ( BrigReq
  , GalleyReq
  , SparReq
  , TestSpar
  , TestEnv(..)
  , ProviderCfg(..)
  , teMgr
  , teCql
  , teBrig
  , teGalley
  , teSpar
  , teMockIdP
  , teSparEnv
  , teOpts
  , teTstOpts
  , Select
  , ResponseLBS
  , IntegrationConfig(..)
  , TestErrorLabel(..)
  ) where

import Imports
import Bilge
import Cassandra as Cas
import Control.Exception
import Control.Lens (Getter, makeLenses, to)
import Data.Aeson
import Data.Aeson.TH
import Data.String.Conversions
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Spar.API ()
import Spar.Types
import Util.Options

import qualified Data.Aeson as Aeson
import qualified Spar.App as Spar


type BrigReq   = Request -> Request
type GalleyReq = Request -> Request
type SparReq   = Request -> Request

type TestSpar = ReaderT TestEnv IO

-- | See 'mkEnv' about what's in here.
data TestEnv = TestEnv
  { _teMgr         :: Manager
  , _teCql         :: Cas.ClientState
  , _teBrig        :: BrigReq
  , _teGalley      :: GalleyReq
  , _teSpar        :: SparReq
  , _teSparEnv     :: Spar.Env
  , _teOpts        :: Opts               -- ^ spar config
  , _teTstOpts     :: IntegrationConfig  -- ^ integration test config
  }

type Select = TestEnv -> (Request -> Request)

type ResponseLBS = Bilge.Response (Maybe LBS)

data IntegrationConfig = IntegrationConfig
  { cfgBrig     :: Endpoint
  , cfgGalley   :: Endpoint
  , cfgSpar     :: Endpoint
  , cfgProvider :: ProviderCfg  -- ^ ("MockIdP" here, but the name is config between all services.)
  } deriving (Show, Generic)

data ProviderCfg = ProviderCfg
    { privateKey   :: FilePath
    , publicKey    :: FilePath
    , cert         :: FilePath
    , botHost      :: Text
    , botPort      :: Int
    } deriving (Show, Generic)

deriveFromJSON deriveJSONOptions ''IntegrationConfig
instance FromJSON ProviderCfg
makeLenses ''TestEnv

teMockIdP :: Getter TestEnv ProviderCfg
teMockIdP = teTstOpts . to cfgProvider


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
