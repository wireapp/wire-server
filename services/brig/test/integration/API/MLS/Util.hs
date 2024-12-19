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
{-# LANGUAGE RecordWildCards #-}

module API.MLS.Util where

import Bilge
import Bilge.Assert
import Data.Aeson (object, toJSON, (.=))
import Data.ByteString.Conversion
import Data.Default
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Qualified
import Data.Text qualified as Text
import Data.Timeout
import Imports
import System.FilePath
import System.Process
import Test.Tasty.HUnit
import Util
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.User.Client

data SetKey = SetKey | DontSetKey
  deriving (Eq)

data KeyingInfo = KeyingInfo
  { kiSetKey :: SetKey,
    kiLifetime :: Maybe Timeout
  }

instance Default KeyingInfo where
  def = KeyingInfo SetKey Nothing

cliCmd :: FilePath -> ClientIdentity -> [String] -> CreateProcess
cliCmd tmp qcid cmnds =
  proc "mls-test-cli" $
    ["--store", tmp </> (show qcid <> ".db")] <> cmnds

initStore ::
  (HasCallStack) =>
  (MonadIO m) =>
  FilePath ->
  ClientIdentity ->
  m ()
initStore tmp qcid = do
  void . liftIO . flip spawn Nothing $
    cliCmd tmp qcid ["init", show qcid]

generateKeyPackage ::
  (HasCallStack) =>
  (MonadIO m) =>
  FilePath ->
  ClientIdentity ->
  Maybe Timeout ->
  m (RawMLS KeyPackage, KeyPackageRef)
generateKeyPackage tmp qcid lifetime = do
  kp <-
    liftIO $
      decodeMLSError <=< flip spawn Nothing $
        cliCmd tmp qcid $
          ["key-package", "create"]
            <> (("--lifetime " <>) . show . (#> Second) <$> maybeToList lifetime)
  let ref = fromJust (kpRef' kp)
  pure (kp, ref)

uploadKeyPackages ::
  (HasCallStack) =>
  Brig ->
  FilePath ->
  KeyingInfo ->
  Qualified UserId ->
  ClientId ->
  Int ->
  Http ()
uploadKeyPackages brig tmp KeyingInfo {..} u c n = do
  let cid = mkClientIdentity u c
  initStore tmp cid
  kps <- replicateM n (fst <$> generateKeyPackage tmp cid kiLifetime)
  when (kiSetKey == SetKey) $
    do
      pk <-
        liftIO . flip spawn Nothing $
          cliCmd tmp cid ["public-key"]
      put
        ( brig
            . paths ["clients", toByteString' c]
            . zUser (qUnqualified u)
            . json defUpdateClient {updateClientMLSPublicKeys = Map.fromList [(Ed25519, pk)]}
        )
      !!! const 200
        === statusCode
  let upload = object ["key_packages" .= toJSON (map (Base64ByteString . raw) kps)]
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' c]
        . zUser (qUnqualified u)
        . json upload
    )
    !!! const (case kiSetKey of SetKey -> 201; DontSetKey -> 400)
      === statusCode

getKeyPackageCount :: (HasCallStack) => Brig -> Qualified UserId -> ClientId -> Http KeyPackageCount
getKeyPackageCount brig u c =
  responseJsonError
    =<< get
      ( brig
          . paths ["mls", "key-packages", "self", toByteString' c, "count"]
          . queryItem "ciphersuite" "0x0001"
          . zUser (qUnqualified u)
      )
      <!! const 200
        === statusCode

decodeMLSError :: (ParseMLS a) => ByteString -> IO a
decodeMLSError s = case decodeMLS' s of
  Left e -> assertFailure ("Could not parse MLS object: " <> Text.unpack e)
  Right x -> pure x
