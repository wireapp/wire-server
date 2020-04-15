{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

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

module TestSetup
  ( IntegrationConfig (..),
    LegalHoldConfig (..),
    tsGConf,
    tsIConf,
    tsManager,
    tsGalley,
    tsBrig,
    tsCannon,
    tsAwsEnv,
    tsMaxConvSize,
    tsCass,
    TestM (..),
    TestSetup (..),
  )
where

import Bilge (Manager, MonadHttp (..), Request, withResponse)
import qualified Cassandra as Cql
import Control.Lens (makeLenses, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fail (MonadFail)
import Data.Aeson
import qualified Galley.Aws as Aws
import Galley.Options (Opts)
import Imports
import Util.Options

type GalleyR = Request -> Request

type BrigR = Request -> Request

type CannonR = Request -> Request

data IntegrationConfig
  = IntegrationConfig
      -- internal endpoints
      { galley :: Endpoint,
        brig :: Endpoint,
        cannon :: Endpoint,
        provider :: LegalHoldConfig
      }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

-- FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
data LegalHoldConfig
  = LegalHoldConfig
      { privateKey :: FilePath,
        publicKey :: FilePath,
        cert :: FilePath,
        botHost :: Text,
        botPort :: Int
      }
  deriving (Show, Generic)

instance FromJSON LegalHoldConfig

data TestSetup
  = TestSetup
      { _tsGConf :: Opts,
        _tsIConf :: IntegrationConfig,
        _tsManager :: Manager,
        _tsGalley :: GalleyR,
        _tsBrig :: BrigR,
        _tsCannon :: CannonR,
        _tsAwsEnv :: Maybe Aws.Env,
        _tsMaxConvSize :: Word16,
        _tsCass :: Cql.ClientState
      }

makeLenses ''TestSetup

newtype TestM a
  = TestM {runTestM :: ReaderT TestSetup IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader TestSetup,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadUnliftIO,
      MonadFail
    )

instance MonadHttp TestM where
  handleRequestWithCont req handler = do
    manager <- view tsManager
    liftIO $ withResponse req manager handler
