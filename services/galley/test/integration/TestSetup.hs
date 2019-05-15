{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module TestSetup
    ( IntegrationConfig(..)
    , LegalHoldConfig(..)
    , test
    , tsGConf
    , tsIConf
    , tsManager
    , tsGalley
    , tsBrig
    , tsCannon
    , tsAwsEnv
    , tsMaxConvSize
    , TestM(..)
    , TestSetup(..)
    ) where

import Imports
import Data.Aeson
import Test.Tasty          (TestName, TestTree)
import Test.Tasty.HUnit    (Assertion, testCase)
import Control.Lens        (makeLenses, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Bilge (Manager, MonadHttp(..), Request, withResponse)
import Util.Options
import Galley.Options      (Opts)

import qualified Galley.Aws          as Aws

type GalleyR      = Request -> Request
type BrigR        = Request -> Request
type CannonR      = Request -> Request

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { galley    :: Endpoint
  , brig      :: Endpoint
  , cannon    :: Endpoint
  , provider  :: LegalHoldConfig
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

-- FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
data LegalHoldConfig = LegalHoldConfig
    { privateKey   :: FilePath
    , publicKey    :: FilePath
    , cert         :: FilePath
    , botHost      :: Text
    , botPort      :: Int
    } deriving (Show, Generic)

instance FromJSON LegalHoldConfig

data TestSetup = TestSetup
    { _tsGConf       :: Opts
    , _tsIConf       :: IntegrationConfig
    , _tsManager     :: Manager
    , _tsGalley      :: GalleyR
    , _tsBrig        :: BrigR
    , _tsCannon      :: CannonR
    , _tsAwsEnv      :: Maybe Aws.Env
    , _tsMaxConvSize :: Word16
    }

makeLenses ''TestSetup

newtype TestM a =
  TestM { runTestM :: ReaderT TestSetup IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TestSetup
             , MonadIO
             , MonadCatch
             , MonadThrow
             , MonadMask
             , MonadUnliftIO
             )

instance MonadHttp TestM where
    handleRequestWithCont req handler = do
        manager <- view tsManager
        liftIO $ withResponse req manager handler

test :: IO TestSetup -> TestName -> TestM a -> TestTree
test s n h = testCase n runTest
  where
    runTest :: Assertion
    runTest = do
        setup <- s
        void . flip runReaderT setup . runTestM $ h
