{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Util.Types
  ( BrigReq,
    GalleyReq,
    SparReq,
    TestSpar,
    TestEnv (..),
    teMgr,
    teCql,
    teBrig,
    teGalley,
    teSpar,
    teSparEnv,
    teOpts,
    teTstOpts,
    Select,
    ResponseLBS,
    IntegrationConfig (..),
    TestErrorLabel (..),
  )
where

import Bilge
import Cassandra as Cas
import Control.Exception
import Control.Lens (makeLenses)
import Crypto.Random.Types (MonadRandom (..))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import Data.String.Conversions
import Imports
import SAML2.WebSSO.Types.TH (deriveJSONOptions)
import Spar.API ()
import qualified Spar.App as Spar
import Spar.Types
import Util.Options

type BrigReq = Request -> Request

type GalleyReq = Request -> Request

type SparReq = Request -> Request

type TestSpar = ReaderT TestEnv IO

instance MonadRandom TestSpar where
  getRandomBytes = lift . getRandomBytes

-- | See 'mkEnv' about what's in here.
data TestEnv = TestEnv
  { _teMgr :: Manager,
    _teCql :: Cas.ClientState,
    _teBrig :: BrigReq,
    _teGalley :: GalleyReq,
    _teSpar :: SparReq,
    _teSparEnv :: Spar.Env,
    -- | spar config
    _teOpts :: Opts,
    -- | integration test config
    _teTstOpts :: IntegrationConfig
  }

type Select = TestEnv -> (Request -> Request)

data IntegrationConfig = IntegrationConfig
  { cfgBrig :: Endpoint,
    cfgGalley :: Endpoint,
    cfgSpar :: Endpoint
  }
  deriving (Show, Generic)

deriveFromJSON deriveJSONOptions ''IntegrationConfig

makeLenses ''TestEnv

newtype TestErrorLabel = TestErrorLabel {fromTestErrorLabel :: ST}
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
    throwIO . ErrorCall . show $
      val
