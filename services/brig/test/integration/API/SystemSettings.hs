{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module API.SystemSettings (tests) where

import Bilge
import Bilge.Assert
import Brig.Options
import Control.Lens.Operators
import Control.Monad.Catch
import qualified Data.Code as Code
import Data.CookieThrottle
import Data.Domain
import Data.EmailVisibility
import Data.Fixed
import Data.ISO3166_CountryCodes as CountryCodes
import Data.LanguageCodes as LanguageCodes
import Data.LimitFailedLogins
import Data.Nonce
import Data.RetryAfter
import Data.SuspendInactiveUsers
import Data.Time
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Routes.Public.Brig
import Wire.API.Team.Feature
import Wire.API.User.Profile
import qualified Wire.Data.Timeout as WireTimeout

tests :: Opts -> Manager -> Brig -> IO TestTree
tests opts m brig = pure $ do
  testGroup
    "settings"
    [ test m "GET /system/settings - mostly unset" $ testGetSettings opts brig testSettings1,
      test m "GET /system/settings - mostly set" $ testGetSettings opts brig testSettings2
    ]

testSettings1 :: SystemSettings
testSettings1 =
  SystemSettings
    { systemSettingsSetActivationTimeout = mkWireTimeout 1,
      systemSettingsSetVerificationCodeTimeoutInternal = Nothing,
      systemSettingsSetTeamInvitationTimeout = mkWireTimeout 2,
      systemSettingsSetExpiredUserCleanupTimeout = Nothing,
      systemSettingsSetUserMaxConnections = 3,
      systemSettingsSetUserMaxPermClients = Nothing,
      systemSettingsSetCookieInsecure = False,
      systemSettingsSetUserCookieRenewAge = 4,
      systemSettingsSetUserCookieLimit = 5,
      systemSettingsSetUserCookieThrottle = StdDevThrottle (StdDev 9.9) (RetryAfter 9),
      systemSettingsSetLimitFailedLogins = Nothing,
      systemSettingsSetSuspendInactiveUsers = Nothing,
      systemSettingsSetRichInfoLimit = 6,
      systemSettingsSetDefaultTemplateLocaleInternal = Nothing,
      systemSettingsSetDefaultUserLocaleInternal = Nothing,
      systemSettingsSetMaxTeamSize = 7,
      systemSettingsSetMaxConvSize = 8,
      systemSettingsSetEmailVisibility = EmailVisibleToSelf,
      systemSettingsSetPropertyMaxKeyLen = Nothing,
      systemSettingsSetPropertyMaxValueLen = Nothing,
      systemSettingsSetDeleteThrottleMillis = Nothing,
      systemSettingsSetSearchSameTeamOnly = Nothing,
      systemSettingsSetFederationDomain = Domain "example.com",
      systemSettingsSetSqsThrottleMillis = Nothing,
      systemSettingsSetRestrictUserCreation = Nothing,
      systemSettingsSetEnableDevelopmentVersions = Nothing,
      systemSettingsSet2FACodeGenerationDelaySecsInternal = Nothing,
      systemSettingsSetNonceTtlSecsInternal = Nothing,
      systemSettingsSetFeatureFlags = Nothing
    }

testSettings2 :: SystemSettings
testSettings2 =
  SystemSettings
    { systemSettingsSetActivationTimeout = mkWireTimeout 1,
      systemSettingsSetVerificationCodeTimeoutInternal = (Just . Code.Timeout . mkNominalDiffTime) 10,
      systemSettingsSetTeamInvitationTimeout = mkWireTimeout 2,
      systemSettingsSetExpiredUserCleanupTimeout = (Just . mkWireTimeout) 11,
      systemSettingsSetUserMaxConnections = 3,
      systemSettingsSetUserMaxPermClients = Just 12,
      systemSettingsSetCookieInsecure = False,
      systemSettingsSetUserCookieRenewAge = 4,
      systemSettingsSetUserCookieLimit = 5,
      systemSettingsSetUserCookieThrottle = StdDevThrottle (StdDev 9.9) (RetryAfter 9),
      systemSettingsSetLimitFailedLogins = Just (LimitFailedLogins (mkWireTimeout 21) 13),
      systemSettingsSetSuspendInactiveUsers = (Just . SuspendInactiveUsers . mkWireTimeout) 14,
      systemSettingsSetRichInfoLimit = 6,
      systemSettingsSetDefaultTemplateLocaleInternal =
        Just
          ( Locale
              { lLanguage = Language LanguageCodes.EN,
                lCountry = Nothing
              }
          ),
      systemSettingsSetDefaultUserLocaleInternal =
        Just
          ( Locale
              { lLanguage = Language LanguageCodes.EN,
                lCountry = Just (Country CountryCodes.GB)
              }
          ),
      systemSettingsSetMaxTeamSize = 7,
      systemSettingsSetMaxConvSize = 8,
      systemSettingsSetEmailVisibility = EmailVisibleIfOnSameTeam,
      systemSettingsSetPropertyMaxKeyLen = Just 15,
      systemSettingsSetPropertyMaxValueLen = Just 16,
      systemSettingsSetDeleteThrottleMillis = Just 17,
      systemSettingsSetSearchSameTeamOnly = Just True,
      systemSettingsSetFederationDomain = Domain "example.com",
      systemSettingsSetSqsThrottleMillis = Just 18,
      systemSettingsSetRestrictUserCreation = Just True,
      systemSettingsSetEnableDevelopmentVersions = Just True,
      systemSettingsSet2FACodeGenerationDelaySecsInternal = Just 19,
      systemSettingsSetNonceTtlSecsInternal = (Just . NonceTtlSecs) 20,
      systemSettingsSetFeatureFlags =
        Just $
          AccountFeatureConfigs
            { afcConferenceCallingDefNew = ImplicitLockStatus (withStatus FeatureStatusEnabled LockStatusUnlocked ConferenceCallingConfig FeatureTTLUnlimited),
              afcConferenceCallingDefNull = ImplicitLockStatus (withStatus FeatureStatusDisabled LockStatusUnlocked ConferenceCallingConfig FeatureTTLUnlimited)
            }
    }

mkWireTimeout :: Integer -> WireTimeout.Timeout
mkWireTimeout = WireTimeout.Timeout . mkNominalDiffTime

mkNominalDiffTime :: Integer -> NominalDiffTime
mkNominalDiffTime = secondsToNominalDiffTime . MkFixed . (* resolution (1 :: Pico))

testGetSettings :: Opts -> Brig -> SystemSettings -> Http ()
testGetSettings opts brig testSettings = do
  let newSettings =
        (opts & optSettings)
          { setActivationTimeout = systemSettingsSetActivationTimeout testSettings,
            setVerificationCodeTimeoutInternal = systemSettingsSetVerificationCodeTimeoutInternal testSettings,
            setTeamInvitationTimeout = systemSettingsSetTeamInvitationTimeout testSettings,
            setExpiredUserCleanupTimeout = systemSettingsSetExpiredUserCleanupTimeout testSettings,
            setUserMaxConnections = systemSettingsSetUserMaxConnections testSettings,
            setUserMaxPermClients = systemSettingsSetUserMaxPermClients testSettings,
            setCookieInsecure = systemSettingsSetCookieInsecure testSettings,
            setUserCookieRenewAge = systemSettingsSetUserCookieRenewAge testSettings,
            setUserCookieLimit = systemSettingsSetUserCookieLimit testSettings,
            setUserCookieThrottle = systemSettingsSetUserCookieThrottle testSettings,
            setLimitFailedLogins = systemSettingsSetLimitFailedLogins testSettings,
            setSuspendInactiveUsers = systemSettingsSetSuspendInactiveUsers testSettings,
            setRichInfoLimit = systemSettingsSetRichInfoLimit testSettings,
            setDefaultTemplateLocaleInternal = systemSettingsSetDefaultTemplateLocaleInternal testSettings,
            setDefaultUserLocaleInternal = systemSettingsSetDefaultUserLocaleInternal testSettings,
            setMaxTeamSize = systemSettingsSetMaxTeamSize testSettings,
            setMaxConvSize = systemSettingsSetMaxConvSize testSettings,
            setEmailVisibility = systemSettingsSetEmailVisibility testSettings,
            setPropertyMaxKeyLen = systemSettingsSetPropertyMaxKeyLen testSettings,
            setPropertyMaxValueLen = systemSettingsSetPropertyMaxValueLen testSettings,
            setDeleteThrottleMillis = systemSettingsSetDeleteThrottleMillis testSettings,
            setSearchSameTeamOnly = systemSettingsSetSearchSameTeamOnly testSettings,
            setFederationDomain = systemSettingsSetFederationDomain testSettings,
            setSqsThrottleMillis = systemSettingsSetSqsThrottleMillis testSettings,
            setRestrictUserCreation = systemSettingsSetRestrictUserCreation testSettings,
            setEnableDevelopmentVersions = systemSettingsSetEnableDevelopmentVersions testSettings,
            set2FACodeGenerationDelaySecsInternal = systemSettingsSet2FACodeGenerationDelaySecsInternal testSettings,
            setNonceTtlSecsInternal = systemSettingsSetNonceTtlSecsInternal testSettings
          }
  let newOpts =
        opts
          { optSettings = newSettings
          }
  queriedSettings <- withSettingsOverrides newOpts $ getSystemSettings brig
  liftIO $ do
    systemSettingsSetRestrictUserCreation queriedSettings @?= systemSettingsSetRestrictUserCreation testSettings
    systemSettingsSetActivationTimeout queriedSettings @?= systemSettingsSetActivationTimeout testSettings
    systemSettingsSetVerificationCodeTimeoutInternal queriedSettings @?= systemSettingsSetVerificationCodeTimeoutInternal testSettings
    systemSettingsSetTeamInvitationTimeout queriedSettings @?= systemSettingsSetTeamInvitationTimeout testSettings
    systemSettingsSetExpiredUserCleanupTimeout queriedSettings @?= systemSettingsSetExpiredUserCleanupTimeout testSettings
    systemSettingsSetUserMaxConnections queriedSettings @?= systemSettingsSetUserMaxConnections testSettings
    systemSettingsSetUserMaxPermClients queriedSettings @?= systemSettingsSetUserMaxPermClients testSettings
    systemSettingsSetCookieInsecure queriedSettings @?= systemSettingsSetCookieInsecure testSettings
    systemSettingsSetUserCookieRenewAge queriedSettings @?= systemSettingsSetUserCookieRenewAge testSettings
    systemSettingsSetUserCookieLimit queriedSettings @?= systemSettingsSetUserCookieLimit testSettings
    systemSettingsSetUserCookieThrottle queriedSettings @?= systemSettingsSetUserCookieThrottle testSettings
    systemSettingsSetLimitFailedLogins queriedSettings @?= systemSettingsSetLimitFailedLogins testSettings
    systemSettingsSetSuspendInactiveUsers queriedSettings @?= systemSettingsSetSuspendInactiveUsers testSettings
    systemSettingsSetRichInfoLimit queriedSettings @?= systemSettingsSetRichInfoLimit testSettings
    systemSettingsSetDefaultTemplateLocaleInternal queriedSettings @?= systemSettingsSetDefaultTemplateLocaleInternal testSettings
    systemSettingsSetDefaultUserLocaleInternal queriedSettings @?= systemSettingsSetDefaultUserLocaleInternal testSettings
    systemSettingsSetMaxTeamSize queriedSettings @?= systemSettingsSetMaxTeamSize testSettings
    systemSettingsSetMaxConvSize queriedSettings @?= systemSettingsSetMaxConvSize testSettings
    systemSettingsSetEmailVisibility queriedSettings @?= systemSettingsSetEmailVisibility testSettings
    systemSettingsSetPropertyMaxKeyLen queriedSettings @?= systemSettingsSetPropertyMaxKeyLen testSettings
    systemSettingsSetPropertyMaxValueLen queriedSettings @?= systemSettingsSetPropertyMaxValueLen testSettings
    systemSettingsSetDeleteThrottleMillis queriedSettings @?= systemSettingsSetDeleteThrottleMillis testSettings
    systemSettingsSetSearchSameTeamOnly queriedSettings @?= systemSettingsSetSearchSameTeamOnly testSettings
    systemSettingsSetFederationDomain queriedSettings @?= systemSettingsSetFederationDomain testSettings
    systemSettingsSetSqsThrottleMillis queriedSettings @?= systemSettingsSetSqsThrottleMillis testSettings
    systemSettingsSetRestrictUserCreation queriedSettings @?= systemSettingsSetRestrictUserCreation testSettings
    systemSettingsSetEnableDevelopmentVersions queriedSettings @?= systemSettingsSetEnableDevelopmentVersions testSettings
    systemSettingsSet2FACodeGenerationDelaySecsInternal queriedSettings @?= systemSettingsSet2FACodeGenerationDelaySecsInternal testSettings
    systemSettingsSetNonceTtlSecsInternal queriedSettings @?= systemSettingsSetNonceTtlSecsInternal testSettings

getSystemSettings ::
  (HasCallStack, MonadIO m, MonadHttp m, MonadCatch m, MonadThrow m) =>
  Brig ->
  m SystemSettings
getSystemSettings brig =
  responseJsonError
    =<< get (brig . path "/system/settings")
      <!! statusCode
        Bilge.Assert.=== const 200
