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

module API.Teams.Feature (tests) where

import API.Util
import API.Util.TeamFeature hiding (getFeatureConfig, setLockStatusInternal)
import Bilge
import Bilge.Assert
import Brig.Types.Test.Arbitrary (Arbitrary (arbitrary))
import Control.Lens.Operators ()
import Data.ByteString.Char8 (unpack)
import Data.Schema (ToSchema)
import GHC.TypeLits (KnownSymbol)
import Imports
import Test.QuickCheck (Gen, generate, suchThat)
import Test.Tasty
import Test.Tasty.HUnit ((@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature hiding (setLockStatus)

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Feature Config API and Team Features API"
    [ testGroup
        "Patch"
        [ -- Note: `SSOConfig` and `LegalHoldConfig` may not be able to be reset
          -- (depending on prior state or configuration). Thus, they cannot be
          -- tested here (setting random values), but are tested with separate
          -- tests.
          test s (unpack $ featureNameBS @SearchVisibilityAvailableConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled SearchVisibilityAvailableConfig,
          test s (unpack $ featureNameBS @ValidateSAMLEmailsConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled ValidateSAMLEmailsConfig,
          test s (unpack $ featureNameBS @DigitalSignaturesConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled DigitalSignaturesConfig,
          test s (unpack $ featureNameBS @AppLockConfig) $
            testPatchWithCustomGen IgnoreLockStatusChange FeatureStatusEnabled (AppLockConfig (EnforceAppLock False) 60) validAppLockConfigGen,
          test s (unpack $ featureNameBS @ConferenceCallingConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled ConferenceCallingConfig,
          test s (unpack $ featureNameBS @SearchVisibilityAvailableConfig) $
            testPatch IgnoreLockStatusChange FeatureStatusEnabled SearchVisibilityAvailableConfig,
          test s (unpack $ featureNameBS @MLSConfig) $
            testPatchWithCustomGen
              AssertLockStatusChange
              FeatureStatusDisabled
              ( MLSConfig
                  []
                  ProtocolProteusTag
                  [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519]
                  MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
                  [ProtocolProteusTag, ProtocolMLSTag]
              )
              validMLSConfigGen,
          test s (unpack $ featureNameBS @FileSharingConfig) $
            testPatch AssertLockStatusChange FeatureStatusEnabled FileSharingConfig,
          test s (unpack $ featureNameBS @GuestLinksConfig) $
            testPatch AssertLockStatusChange FeatureStatusEnabled GuestLinksConfig,
          test s (unpack $ featureNameBS @SndFactorPasswordChallengeConfig) $
            testPatch AssertLockStatusChange FeatureStatusDisabled SndFactorPasswordChallengeConfig,
          test s (unpack $ featureNameBS @SelfDeletingMessagesConfig) $
            testPatch AssertLockStatusChange FeatureStatusEnabled (SelfDeletingMessagesConfig 0),
          test s (unpack $ featureNameBS @OutlookCalIntegrationConfig) $
            testPatch AssertLockStatusChange FeatureStatusDisabled OutlookCalIntegrationConfig,
          test s (unpack $ featureNameBS @MlsE2EIdConfig) $
            testPatchWithArbitrary AssertLockStatusChange FeatureStatusDisabled (wsConfig (defFeatureStatus @MlsE2EIdConfig)),
          test s (unpack $ featureNameBS @EnforceFileDownloadLocationConfig) $
            testPatchWithArbitrary AssertLockStatusChange FeatureStatusDisabled (wsConfig (defFeatureStatus @EnforceFileDownloadLocationConfig))
        ]
    ]

-- | Provides a `Gen` with test objects that are realistic and can easily be asserted
validMLSConfigGen :: Gen (WithStatusPatch MLSConfig)
validMLSConfigGen =
  arbitrary
    `suchThat` ( \cfg ->
                   case wspConfig cfg of
                     Just (MLSConfig us defProtocol cTags ctag supProtocol) ->
                       sortedAndNoDuplicates us
                         && sortedAndNoDuplicates cTags
                         && elem ctag cTags
                         && notElem ProtocolMixedTag supProtocol
                         && elem defProtocol supProtocol
                         && sortedAndNoDuplicates supProtocol
                     _ -> True
                     && Just FeatureStatusEnabled == wspStatus cfg
               )
  where
    sortedAndNoDuplicates xs = (sort . nub) xs == xs

validAppLockConfigGen :: Gen (WithStatusPatch AppLockConfig)
validAppLockConfigGen =
  arbitrary
    `suchThat` ( \cfg -> case wspConfig cfg of
                   Just (AppLockConfig _ secs) -> secs >= 30
                   Nothing -> True
               )

-- | Binary type to prevent "boolean blindness"
data AssertLockStatusChange = AssertLockStatusChange | IgnoreLockStatusChange
  deriving (Eq)

testPatchWithArbitrary ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg),
    Arbitrary (WithStatusPatch cfg)
  ) =>
  AssertLockStatusChange ->
  FeatureStatus ->
  cfg ->
  TestM ()
testPatchWithArbitrary assertLockStatusChange featureStatus cfg = do
  generatedConfig <- liftIO $ generate arbitrary
  testPatch' assertLockStatusChange generatedConfig featureStatus cfg

testPatchWithCustomGen ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg)
  ) =>
  AssertLockStatusChange ->
  FeatureStatus ->
  cfg ->
  Gen (WithStatusPatch cfg) ->
  TestM ()
testPatchWithCustomGen assertLockStatusChange featureStatus cfg gen = do
  generatedConfig <- liftIO $ generate gen
  testPatch' assertLockStatusChange generatedConfig featureStatus cfg

testPatch ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg),
    Arbitrary (WithStatusPatch cfg)
  ) =>
  AssertLockStatusChange ->
  FeatureStatus ->
  cfg ->
  TestM ()
testPatch assertLockStatusChange status cfg = testPatchWithCustomGen assertLockStatusChange status cfg arbitrary

testPatch' ::
  forall cfg.
  ( HasCallStack,
    IsFeatureConfig cfg,
    Typeable cfg,
    ToSchema cfg,
    Eq cfg,
    Show cfg,
    KnownSymbol (FeatureSymbol cfg)
  ) =>
  AssertLockStatusChange ->
  WithStatusPatch cfg ->
  FeatureStatus ->
  cfg ->
  TestM ()
testPatch' testLockStatusChange rndFeatureConfig defStatus defConfig = do
  (uid, tid) <- createBindingTeam
  Just original <- responseJsonMaybe <$> getTeamFeatureInternal @cfg tid
  patchTeamFeatureInternal tid rndFeatureConfig !!! statusCode === const 200
  Just actual <- responseJsonMaybe <$> getTeamFeatureInternal @cfg tid
  liftIO $
    if wsLockStatus actual == LockStatusLocked
      then do
        wsStatus actual @?= defStatus
        wsConfig actual @?= defConfig
      else do
        wsStatus actual @?= fromMaybe (wsStatus original) (wspStatus rndFeatureConfig)
        when (testLockStatusChange == AssertLockStatusChange) $
          wsLockStatus actual @?= fromMaybe (wsLockStatus original) (wspLockStatus rndFeatureConfig)
        wsConfig actual @?= fromMaybe (wsConfig original) (wspConfig rndFeatureConfig)
  checkTeamFeatureAllEndpoints uid tid actual
