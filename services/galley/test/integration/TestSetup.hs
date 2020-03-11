{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module TestSetup
  ( IntegrationConfig (..),
    LegalHoldConfig (..),
    tsGConf,
    tsIConf,
    tsManager,
    tsGalley,
    tsBrig,
    tsFederatedBrig1,
    tsFederatedBrig2,
    tsFederatedBackend1,
    tsFederatedBackend2,
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

-- TODO: federatedBackend1 and federatedBackend2 should probably not be here,
-- but be in Galley configuration. move them out once we have supported to add
-- known backends to galley.
data IntegrationConfig
  = IntegrationConfig
      -- internal endpoints
      { galley :: Endpoint,
        brig :: Endpoint,
        cannon :: Endpoint,
        provider :: LegalHoldConfig,
        federatedBrig1 :: Endpoint, -- An escape hatch into brig on the other side. Dirty. to create users!
        federatedBrig2 :: Endpoint, -- An escape hatch into brig on the other side. Dirty. to create users!
        federatedBackend1 :: Endpoint, -- A backend that we federate with
        federatedBackend2 :: Endpoint -- another backend that we federate with
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
        _tsCass :: Cql.ClientState,
        _tsFederatedBrig1 :: BrigR,
        _tsFederatedBrig2 :: BrigR,
        _tsFederatedBackend1 :: Request -> Request,
        _tsFederatedBackend2 :: Request -> Request
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
