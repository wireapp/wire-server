{-# LANGUAGE TemplateHaskell #-}

module Wire.MlsKeyPackageStore where

import Data.Id
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage

data MlsKeyPackageStore m a where
  InsertKeyPackages :: UserId -> ClientId -> [(KeyPackageRef, CipherSuiteTag, KeyPackageData)] -> MlsKeyPackageStore m ()
  LookupKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> MlsKeyPackageStore m [(KeyPackageRef, KeyPackageData)]
  DeleteKeyPackages :: UserId -> ClientId -> CipherSuiteTag -> [KeyPackageRef] -> MlsKeyPackageStore m ()
  DeleteAllKeyPackages :: UserId -> ClientId -> [CipherSuiteTag] -> MlsKeyPackageStore m ()
  DeleteKeyPackage :: UserId -> ClientId -> CipherSuiteTag -> KeyPackageRef -> MlsKeyPackageStore m ()

makeSem ''MlsKeyPackageStore
