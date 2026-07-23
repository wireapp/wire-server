{-# LANGUAGE TemplateHaskell #-}

module Wire.MlsKeyPackageSubsystem where

import Data.Id
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Lifetime

data MlsKeyPackageSubsystem m a where
  InsertMlsKeyPackages :: UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> MlsKeyPackageSubsystem m ()
  ClaimMlsKeyPackage :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageSubsystem m (Maybe (KeyPackageRef, KeyPackageData))
  CountMlsKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageSubsystem m Int64
  DeleteMlsKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> [KeyPackageRef] -> MlsKeyPackageSubsystem m ()
  DeleteAllMlsKeyPackages :: UserId -> ClientId -> [CipherSuiteTag] -> MlsKeyPackageSubsystem m ()

makeSem ''MlsKeyPackageSubsystem

validateKeyPackageLifetime :: POSIXTime -> Maybe NominalDiffTime -> Lifetime -> Either Text ()
validateKeyPackageLifetime now maxLifetime lifetime = do
  when (tsPOSIX lifetime.ltNotBefore > now) $ Left "Key package not_before date is in the future"
  when (tsPOSIX lifetime.ltNotAfter <= now) $ Left "Key package is expired"
  for_ maxLifetime $ \maxAge -> when (tsPOSIX lifetime.ltNotAfter > now + maxAge) $ Left "Key package expiration time is too far in the future"
