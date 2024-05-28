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
import API.Util.TeamFeature qualified as Util
import Bilge
import Bilge.Assert
import Brig.Types.Test.Arbitrary (Arbitrary (arbitrary))
import Control.Lens (view)
import Control.Lens.Operators ()
import Control.Monad.Catch (MonadCatch)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 (unpack)
import Data.List1 qualified as List1
import Data.Schema (ToSchema)
import Data.Timeout (TimeoutUnit (Second), (#))
import GHC.TypeLits (KnownSymbol)
import Imports
import Network.Wai.Utilities (label)
import Test.QuickCheck (Gen, generate, suchThat)
import Test.Tasty
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit ((@?=))
import TestHelpers (test)
import TestSetup
import Wire.API.Conversation.Protocol
import Wire.API.Event.FeatureConfig qualified as FeatureConfig
import Wire.API.Internal.Notification (Notification)
import Wire.API.MLS.CipherSuite
import Wire.API.Team.Feature hiding (setLockStatus)

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Feature Config API and Team Features API"
    [ test s "MlsE2EId feature config" $
        testNonTrivialConfigNoTTL
          ( withStatus
              FeatureStatusDisabled
              LockStatusUnlocked
              (wsConfig (defFeatureStatus @MlsE2EIdConfig))
              FeatureTTLUnlimited
          ),
      testGroup
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

testNonTrivialConfigNoTTL ::
  forall cfg.
  ( Typeable cfg,
    Eq cfg,
    Show cfg,
    ToSchema cfg,
    IsFeatureConfig cfg,
    KnownSymbol (FeatureSymbol cfg),
    Arbitrary (WithStatus cfg)
  ) =>
  WithStatus cfg ->
  TestM ()
testNonTrivialConfigNoTTL defaultCfg = do
  (owner, tid, member : _) <- createBindingTeamWithNMembers 1
  nonMember <- randomUser

  galley <- viewGalley
  cannon <- view tsCannon

  let getForTeam :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getForTeam expected =
        flip assertFlagWithConfig expected $ getTeamFeature @cfg member tid

      getForTeamInternal :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getForTeamInternal expected =
        flip assertFlagWithConfig expected $ getTeamFeatureInternal @cfg tid

      getForUser :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getForUser expected = do
        result <- Util.getFeatureConfig @cfg member
        liftIO $ wsStatus result @?= wssStatus expected
        liftIO $ wsConfig result @?= wssConfig expected

      getViaEndpoints :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      getViaEndpoints expected = do
        getForTeam expected
        getForTeamInternal expected
        getForUser expected

      setForTeam :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      setForTeam wsnl =
        putTeamFeature @cfg owner tid wsnl
          !!! statusCode
            === const 200

      setForTeamInternal :: HasCallStack => WithStatusNoLock cfg -> TestM ()
      setForTeamInternal wsnl =
        void $ putTeamFeatureInternal @cfg expect2xx tid wsnl
      setLockStatus :: LockStatus -> TestM ()
      setLockStatus lockStatus =
        Util.setLockStatusInternal @cfg galley tid lockStatus
          !!! statusCode
            === const 200

  assertFlagForbidden $ getTeamFeature @cfg nonMember tid

  getViaEndpoints (forgetLock defaultCfg)

  -- unlock feature
  setLockStatus LockStatusUnlocked

  let defaultMLSConfig =
        WithStatusNoLock
          { wssStatus = FeatureStatusEnabled,
            wssConfig =
              MLSConfig
                { mlsProtocolToggleUsers = [],
                  mlsDefaultProtocol = ProtocolMLSTag,
                  mlsAllowedCipherSuites = [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519],
                  mlsDefaultCipherSuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519,
                  mlsSupportedProtocols = [ProtocolProteusTag, ProtocolMLSTag]
                },
            wssTTL = FeatureTTLUnlimited
          }

  config2 <- liftIO $ generate arbitrary <&> (forgetLock . setTTL FeatureTTLUnlimited)
  config3 <- liftIO $ generate arbitrary <&> (forgetLock . setTTL FeatureTTLUnlimited)

  putTeamFeature @MLSConfig owner tid defaultMLSConfig
    !!! statusCode
      === const 200

  WS.bracketR cannon member $ \ws -> do
    setForTeam config2
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate @cfg config2 LockStatusUnlocked
  getViaEndpoints config2

  WS.bracketR cannon member $ \ws -> do
    setForTeamInternal config3
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertFeatureConfigUpdate @cfg config3 LockStatusUnlocked
  getViaEndpoints config3

  -- lock the feature
  setLockStatus LockStatusLocked
  -- feature status should now be the default again
  getViaEndpoints (forgetLock defaultCfg)
  -- unlock feature
  setLockStatus LockStatusUnlocked
  -- feature status should be the previously set value
  getViaEndpoints config3

assertFlagForbidden :: HasCallStack => TestM ResponseLBS -> TestM ()
assertFlagForbidden res = do
  res !!! do
    statusCode === const 403
    fmap label . responseJsonMaybe === const (Just "no-team-member")

assertFlagWithConfig ::
  forall cfg m.
  ( HasCallStack,
    Eq cfg,
    ToSchema cfg,
    Show cfg,
    Typeable cfg,
    IsFeatureConfig cfg,
    MonadIO m,
    MonadCatch m
  ) =>
  m ResponseLBS ->
  WithStatusNoLock cfg ->
  m ()
assertFlagWithConfig response expected = do
  r <- response
  let rJson = responseJsonEither @(WithStatusNoLock cfg) r
  pure r !!! statusCode === const 200
  liftIO $ do
    fmap wssStatus rJson @?= (Right . wssStatus $ expected)
    fmap wssConfig rJson @?= (Right . wssConfig $ expected)

wsAssertFeatureConfigUpdate ::
  forall cfg.
  ( KnownSymbol (FeatureSymbol cfg),
    ToJSON (WithStatus cfg)
  ) =>
  WithStatusNoLock cfg ->
  LockStatus ->
  Notification ->
  IO ()
wsAssertFeatureConfigUpdate config lockStatus notification = do
  let e :: FeatureConfig.Event = List1.head (WS.unpackPayload notification)
  FeatureConfig._eventType e @?= FeatureConfig.Update
  FeatureConfig._eventFeatureName e @?= featureName @cfg
  FeatureConfig._eventData e @?= Aeson.toJSON (withLockStatus lockStatus config)
