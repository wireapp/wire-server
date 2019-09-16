{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Types
  ( BrigReq
  , GalleyReq
  , SparReq
  , TestSpar
  , TestEnv(..)
  , teMgr
  , teCql
  , teBrig
  , teGalley
  , teSpar
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
import Control.Lens (makeLenses)
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

data IntegrationConfig = IntegrationConfig
  { cfgBrig    :: Endpoint
  , cfgGalley  :: Endpoint
  , cfgSpar    :: Endpoint
  } deriving (Show, Generic)

deriveFromJSON deriveJSONOptions ''IntegrationConfig
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
