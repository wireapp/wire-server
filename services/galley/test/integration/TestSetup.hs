{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    tsUnversionedGalley,
    tsUnversionedBrig,
    tsCannon,
    tsAwsEnv,
    tsMaxConvSize,
    tsCass,
    tsFedGalleyClient,
    tsTeamEventWatcher,
    TestM (..),
    TestSetup (..),
    FedClient (..),
    runFedClient,
    GalleyR,
    BrigR,
    CannonR,
  )
where

import Bilge (Manager, MonadHttp (..), Request, withResponse)
import qualified Cassandra as Cql
import Control.Lens (makeLenses, view, (^.))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Domain
import Data.Proxy
import qualified Data.Text as Text
import GHC.TypeLits
import qualified Galley.Aws as Aws
import Galley.Options (Opts)
import Imports
import qualified Network.HTTP.Client as HTTP
import Proto.TeamEvents (TeamEvent)
import qualified Servant.Client as Servant
import qualified Servant.Client.Core as Servant
import Test.Tasty.HUnit
import Util.Options
import qualified Util.Test.SQS as SQS
import Wire.API.Federation.API
import Wire.API.Federation.Domain

type GalleyR = Request -> Request

type BrigR = Request -> Request

type CannonR = Request -> Request

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { galley :: Endpoint,
    brig :: Endpoint,
    cannon :: Endpoint,
    provider :: LegalHoldConfig
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

-- FUTUREWORK: reduce duplication (copied from brig/Provider.hs)
data LegalHoldConfig = LegalHoldConfig
  { privateKey :: FilePath,
    publicKey :: FilePath,
    cert :: FilePath,
    botHost :: Text
  }
  deriving (Show, Generic)

instance FromJSON LegalHoldConfig

newtype TestM a = TestM {runTestM :: ReaderT TestSetup IO a}
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

data FedClient (comp :: Component) = FedClient HTTP.Manager Endpoint

data TestSetup = TestSetup
  { _tsGConf :: Opts,
    _tsIConf :: IntegrationConfig,
    _tsManager :: Manager,
    _tsUnversionedGalley :: GalleyR,
    _tsUnversionedBrig :: BrigR,
    _tsCannon :: CannonR,
    _tsAwsEnv :: Maybe Aws.Env,
    _tsMaxConvSize :: Word16,
    _tsCass :: Cql.ClientState,
    _tsFedGalleyClient :: FedClient 'Galley,
    _tsTeamEventWatcher :: Maybe (SQS.SQSWatcher TeamEvent)
  }

makeLenses ''TestSetup

instance MonadHttp TestM where
  handleRequestWithCont req handler = do
    manager <- view tsManager
    liftIO $ withResponse req manager handler

runFedClient ::
  forall (name :: Symbol) comp m api.
  ( HasUnsafeFedEndpoint comp api name,
    Servant.HasClient Servant.ClientM api,
    MonadIO m,
    HasCallStack
  ) =>
  FedClient comp ->
  Domain ->
  Servant.Client m api
runFedClient (FedClient mgr endpoint) domain =
  Servant.hoistClient (Proxy @api) (servantClientMToHttp domain) $
    Servant.clientIn (Proxy @api) (Proxy @Servant.ClientM)
  where
    servantClientMToHttp :: Domain -> Servant.ClientM a -> m a
    servantClientMToHttp originDomain action = liftIO $ do
      let host = Text.unpack $ endpoint ^. epHost
          port = fromInteger . toInteger $ endpoint ^. epPort
          baseUrl = Servant.BaseUrl Servant.Http host port "/federation"
          clientEnv = Servant.ClientEnv mgr baseUrl Nothing (makeClientRequest originDomain)
      eitherRes <- Servant.runClientM action clientEnv
      case eitherRes of
        Right res -> pure res
        Left err -> assertFailure $ "Servant client failed with: " <> show err

    makeClientRequest :: Domain -> Servant.BaseUrl -> Servant.Request -> HTTP.Request
    makeClientRequest originDomain burl req =
      let req' = Servant.defaultMakeClientRequest burl req
       in req'
            { HTTP.requestHeaders =
                HTTP.requestHeaders req' <> [(originDomainHeaderName, toByteString' originDomain)]
            }
