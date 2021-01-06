{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

-- FUTUREWORK: this is all copied from galley and spar. Maybe at some point reduce duplication?
module Test.Federator.Util where

-- import Bilge hiding (getCookie) -- we use Web.Cookie instead of the http-client type
-- import Bilge.Assert (Assertions, (!!!), (<!!), (===))
-- import qualified Brig.Types.Activation as Brig
-- import Brig.Types.Common (UserIdentity (..), UserSSOId (..))
-- import Brig.Types.User (User (..), selfUser, userIdentity)
-- import qualified Brig.Types.User as Brig
-- import qualified Brig.Types.User.Auth as Brig
-- import Control.Exception
-- import Control.Lens hiding ((.=))
-- import Control.Monad.Catch
-- import Control.Monad.Except
-- import Control.Retry
import Crypto.Random.Types (MonadRandom)
-- import Data.Aeson as Aeson hiding (json)
-- import Data.Aeson.Lens as Aeson
-- import qualified Data.ByteString as SBS
-- import qualified Data.ByteString.Base64.Lazy as EL
-- import Data.ByteString.Conversion
-- import Data.Handle (Handle (Handle))
-- import Data.Id
-- import Data.Misc (PlainTextPassword (..))
-- import Data.Proxy
-- import Data.Range
-- import Data.String.Conversions
-- import qualified Data.Text.Ascii as Ascii
-- import Data.Text.Encoding (encodeUtf8)
-- import Data.Time
-- import Data.UUID as UUID hiding (fromByteString, null)
-- import Data.UUID.V4 as UUID (nextRandom)
-- import qualified Data.Yaml as Yaml
-- import GHC.TypeLits
-- import qualified Galley.Types.Teams as Galley
import Imports hiding (head)
-- import Network.HTTP.Client.MultipartFormData
-- import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Network.Wai.Handler.Warp.Internal as Warp
-- import qualified Options.Applicative as OPA
-- import qualified System.Logger.Extended as Log
-- import System.Random (randomRIO)
import Test.Hspec hiding (it, pending, pendingWith, xit)

-- import qualified Test.Hspec
-- import qualified Text.XML as XML
-- import qualified Text.XML.Cursor as XML
-- import Text.XML.DSig (SignPrivCreds)
-- import qualified Text.XML.DSig as SAML
-- import URI.ByteString
-- import Util.Options
-- import Util.Types
-- import qualified Web.Cookie as Web
-- import Wire.API.Team.Feature (TeamFeatureStatusValue (..))
-- import qualified Wire.API.Team.Feature as Public
-- import qualified Wire.API.Team.Invitation as TeamInvitation
-- import qualified Wire.API.User as User

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

-- | Call 'mkEnv' with options from config files.
mkEnvFromOptions :: IO TestEnv
mkEnvFromOptions = do
  let desc = "Spar - SSO Service Integration Test Suite"
  (integrationCfgFilePath, cfgFilePath) <- OPA.execParser (OPA.info (OPA.helper <*> cliOptsParser) (OPA.header desc <> OPA.fullDesc))
  integrationOpts :: IntegrationConfig <- Yaml.decodeFileEither integrationCfgFilePath >>= either (error . show) pure
  serviceOpts :: Opts <- Yaml.decodeFileEither cfgFilePath >>= either (throwIO . ErrorCall . show) Spar.Options.deriveOpts
  mkEnv integrationOpts serviceOpts

-- | Accept config file locations as cli options.
cliOptsParser :: OPA.Parser (String, String)
cliOptsParser =
  (,)
    <$> ( OPA.strOption $
            OPA.long "integration-config"
              <> OPA.short 'i'
              <> OPA.help "Integration config to load"
              <> OPA.showDefault
              <> OPA.value defaultIntPath
        )
    <*> ( OPA.strOption $
            OPA.long "service-config"
              <> OPA.short 's'
              <> OPA.help "Spar application config to load"
              <> OPA.showDefault
              <> OPA.value defaultSparPath
        )
  where
    defaultIntPath = "/etc/wire/integration/integration.yaml"
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"

-- | Create an environment for integration tests from integration and spar config files.
--
-- NB: We used to have a mock IdP server here that allowed spar to resolve metadata URLs and pull
-- metadata.  (It *could* have been used by the test suite to get 'AuthnRequest' values as well, but
-- that's no more interesting than simulating the idp end-point from inside the spar-integration
-- executable as a monadic function, only more complicated.)  Since spar does not accept metadata
-- URLs any more <https://github.com/wireapp/wire-server/pull/466#issuecomment-419396359>, we
-- removed the mock idp functionality.  if you want to re-introduce it,
-- <https://github.com/wireapp/wire-server/pull/466/commits/9c93f1e278500522a0565639140ac55dc21ee2d2>
-- would be a good place to look for code to steal.
mkEnv :: HasCallStack => IntegrationConfig -> Opts -> IO TestEnv
mkEnv _teTstOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  -- sparCtxLogger <- Log.mkLogger (toLevel $ saml _teOpts ^. SAML.cfgLogLevel) (logNetStrings _teOpts) (logFormat _teOpts)
  let _teBrig = endpointToReq (cfgBrig _teTstOpts)
      _teGalley = endpointToReq (cfgGalley _teTstOpts)
      _teSpar = endpointToReq (cfgSpar _teTstOpts)
      _teSparEnv = Spar.Env {..}
      sparCtxOpts = _teOpts
      sparCtxCas = _teCql
      sparCtxHttpManager = _teMgr
      sparCtxHttpBrig = _teBrig empty
      sparCtxHttpGalley = _teGalley empty
      sparCtxRequestId = RequestId "<fake request id>"
  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
