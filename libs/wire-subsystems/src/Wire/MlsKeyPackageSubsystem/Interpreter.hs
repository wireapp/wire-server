module Wire.MlsKeyPackageSubsystem.Interpreter (interpretMlsKeyPackageSubsystem) where

import Control.Concurrent qualified as C
import Control.Monad.Random (randomRIO)
import Data.Id
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX
import Imports
import Polysemy
import Polysemy.Resource (Resource, bracket)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Serialisation
import Wire.MlsKeyPackageStore qualified as Store
import Wire.MlsKeyPackageSubsystem

interpretMlsKeyPackageSubsystem :: (Member Store.MlsKeyPackageStore r, Member (Embed IO) r, Member Resource r) => Maybe NominalDiffTime -> C.MVar () -> InterpreterFor MlsKeyPackageSubsystem r
interpretMlsKeyPackageSubsystem configuredLifetime lock = interpret $ \case
  InsertMlsKeyPackages u c ps -> Store.insertKeyPackages u c ps
  ClaimMlsKeyPackage u c s -> bracket (embed (C.takeMVar lock)) (const $ embed (C.putMVar lock ())) (const $ claim configuredLifetime u c s)
  CountMlsKeyPackages u c s -> fromIntegral . length <$> available configuredLifetime u c s
  DeleteMlsKeyPackages u c s rs -> Store.deleteKeyPackages u c s rs
  DeleteAllMlsKeyPackages u c ss -> Store.deleteAllKeyPackages u c ss

claim :: (Member Store.MlsKeyPackageStore r, Member (Embed IO) r) => Maybe NominalDiffTime -> UserId -> ClientId -> CipherSuiteTag -> Sem r (Maybe (KeyPackageRef, KeyPackageData))
claim maxLifetime u c s = do
  candidates <- available maxLifetime u c s
  case candidates of
    [] -> pure Nothing
    _ -> do
      index <- embed $ randomRIO (0, length candidates - 1)
      let candidate = candidates !! index
      Store.deleteKeyPackage u c s (fst candidate)
      pure (Just candidate)

available :: (Member Store.MlsKeyPackageStore r, Member (Embed IO) r) => Maybe NominalDiffTime -> UserId -> ClientId -> CipherSuiteTag -> Sem r [(KeyPackageRef, KeyPackageData)]
available maxLifetime u c s = do
  rows <- Store.lookupKeyPackages u c s
  now <- embed getPOSIXTime
  let decoded = mapMaybe decode rows
      (expired, usable) = partition (isExpired now maxLifetime) decoded
  Store.deleteKeyPackages u c s (map (fst . snd) expired)
  pure (map snd usable)
  where
    decode :: (KeyPackageRef, KeyPackageData) -> Maybe (KeyPackage, (KeyPackageRef, KeyPackageData))
    decode row@(_, packageData) = do
      package <- either (const Nothing) Just (decodeMLS' (kpData packageData) :: Either Text (RawMLS KeyPackage))
      pure (package.value, row)
    isExpired now configuredLifetime (package, _) = case package.leafNode.source of
      LeafNodeSourceKeyPackage lifetime -> isLeft (validateKeyPackageLifetime now configuredLifetime lifetime)
      _ -> True
