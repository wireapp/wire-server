{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
    teWireIdPAPIVersion,
    Select,
    ResponseLBS,
    IntegrationConfig (..),
    TestErrorLabel (..),
    skipIdPAPIVersions,
  )
where

import Bilge
import Cassandra as Cas
import Control.Lens (makeLenses, view)
import Crypto.Random.Types (MonadRandom (..))
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import Imports
import Spar.API ()
import qualified Spar.App as Spar
import Spar.Options
import Test.Hspec (pendingWith)
import Util.Options
import Wire.API.User.IdentityProvider (WireIdPAPIVersion)

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
    _teTstOpts :: IntegrationConfig,
    -- | If True, run tests against legacy SAML API where team is derived from idp issuer
    -- instead of teamid.  See Section "using the same IdP (same entityID, or Issuer) with
    -- different teams" in "/docs/reference/spar-braindump.md" for more details.
    --
    -- NB: this has no impact on the tested spar code; the rest API supports both legacy and
    -- multi-sp mode.  this falg merely determines how the rest API is used.
    _teWireIdPAPIVersion :: WireIdPAPIVersion
  }

type Select = TestEnv -> (Request -> Request)

data IntegrationConfig = IntegrationConfig
  { brig :: Endpoint,
    galley :: Endpoint,
    spar :: Endpoint,
    brigSettingsTeamInvitationTimeout :: Int
  }
  deriving (Show, Generic)

deriveFromJSON Aeson.defaultOptions ''IntegrationConfig

makeLenses ''TestEnv

newtype TestErrorLabel = TestErrorLabel {fromTestErrorLabel :: Text}
  deriving (Eq, Show, IsString)

instance FromJSON TestErrorLabel where
  parseJSON = fmap TestErrorLabel . withObject "TestErrorLabel" (.: "label")

-- | FUTUREWORK(fisx): we're running all tests for all constructors of `WireIdPAPIVersion`,
-- which sometimes makes little sense.  'skipIdPAPIVersions' can be used to pend individual
-- tests that do not even work in both versions (most of them should), or ones that aren't
-- that interesting to run twice (like if SAML is not involved at all).  A more scalable
-- solution would be to pass the versions that a test should be run on as an argument to
-- describe ('skipIdPAPIVersions' only works on individual leafs of the test tree, not on
-- sub-trees), but that would be slightly (only slightly) more involved than I would like.
-- so, some other time.  (Context: `make -C services/spar i` takes currently takes 3m22.476s
-- on my laptop, including all the uninteresting tests.  So this is the maximum time
-- improvement that we can get out of this.)
skipIdPAPIVersions :: (MonadIO m, MonadReader TestEnv m) => [WireIdPAPIVersion] -> m ()
skipIdPAPIVersions skip = do
  view teWireIdPAPIVersion >>= \vers -> when (vers `elem` skip) . liftIO $ do
    pendingWith $ "skipping " <> show vers <> " for this test case (behavior covered by other versions)"
