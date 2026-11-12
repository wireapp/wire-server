-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.BackupBlobStore.KeySpec (spec) where

import Data.ByteString qualified as BS
import Data.Id
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.HttpApiData (toUrlPiece)
import Wire.API.Backup
import Wire.BackupBlobStore.Key

testKey :: BackupHmacKey
testKey = fromRight (error "invalid test key") (mkBackupHmacKey (BS.replicate 32 0x0b))

uid1 :: UserId
uid1 = Id (fromJust (UUID.fromText "00000000-0000-0000-0000-000000000001"))

cid1 :: ClientId
cid1 = ClientId 0x1a2b

fid1 :: BackupFileId
fid1 = BackupFileId (fromJust (UUID.fromText "018f0000-0000-7000-8000-000000000000"))

loc1 :: BackupLocation
loc1 = BackupLocation {user = uid1, client = cid1, epoch = BackupEpoch 7, kind = BackupFull, fileId = fid1}

genLoc :: Gen BackupLocation
genLoc =
  BackupLocation
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (BackupFileId <$> arbitrary)

fidText :: Text
fidText = "018f0000-0000-7000-8000-000000000000"

spec :: Spec
spec = do
  describe "mkBackupHmacKey" $ do
    it "rejects < 32 bytes" $
      isLeft (mkBackupHmacKey (BS.replicate 31 0)) `shouldBe` True
    it "accepts 32 bytes" $
      isRight (mkBackupHmacKey (BS.replicate 32 0)) `shouldBe` True
    it "Show does not leak the key" $
      show testKey `shouldBe` "<redacted>"

  describe "renderKey" $ do
    it "matches known vector" $
      renderKey testKey loc1
        `shouldBe` "backup/f72fd43a3383faef21b364b0df978c5ee52e44557a8a565f7f0a7f6992a4cd8c/ce8a59f6f5c32a3473c2acaa1e647ee9b3885a6326bc91334f9ee80c3fe97637/7/full/018f0000-0000-7000-8000-000000000000"
    it "does not contain uid or cid" $ do
      let k = renderKey testKey loc1
      T.isInfixOf "00000000-0000-0000-0000-000000000001" k `shouldBe` False
      T.isInfixOf "1a2b" k `shouldBe` False
    it "epoch 10 is not under epoch 1 prefix" $
      renderPrefix testKey (EpochPrefix uid1 cid1 (BackupEpoch 1))
        `shouldSatisfy` (not . (`T.isPrefixOf` renderKey testKey loc1 {epoch = BackupEpoch 10}))

  describe "renderPrefix" $ do
    prop "always ends with /" $ \u c e ->
      all
        ((== "/") . T.takeEnd 1 . renderPrefix testKey)
        [UserPrefix u, ClientPrefix u c, EpochPrefix u c e]
    prop "prefixes nest" $ \u c e ->
      renderPrefix testKey (UserPrefix u)
        `T.isPrefixOf` renderPrefix testKey (ClientPrefix u c)
        && renderPrefix testKey (ClientPrefix u c)
          `T.isPrefixOf` renderPrefix testKey (EpochPrefix u c e)

  describe "parseFileKey" $ do
    prop "round-trips renderKey" $ forAll genLoc $ \loc ->
      parseFileKey
        (renderPrefix testKey (EpochPrefix loc.user loc.client loc.epoch))
        (renderKey testKey loc)
        === Just (loc.kind, loc.fileId)
    let p = renderPrefix testKey (EpochPrefix uid1 cid1 (BackupEpoch 7))
    it "rejects other kinds" $
      parseFileKey p (p <> "diff/" <> fidText) `shouldBe` Nothing
    it "rejects invalid uuid" $
      parseFileKey p (p <> "full/not-a-uuid") `shouldBe` Nothing
    it "rejects extra segments" $
      parseFileKey p (p <> "full/" <> fidText <> "/x") `shouldBe` Nothing
    it "rejects different prefix" $
      parseFileKey p (renderKey testKey loc1 {epoch = BackupEpoch 8}) `shouldBe` Nothing
    it "accepts valid" $
      parseFileKey p (p <> "incremental/" <> fidText) `shouldBe` Just (BackupIncremental, fid1)

  describe "parseEpochPrefix" $ do
    prop "round-trips" $ \e ->
      let cp = renderPrefix testKey (ClientPrefix uid1 cid1)
       in parseEpochPrefix cp (cp <> toUrlPiece e <> "/") === Just e
    let cp = renderPrefix testKey (ClientPrefix uid1 cid1)
    it "rejects invalid" $
      forM_ ["0/", "x/", "4294967296/", "7", "07/", "+7/", "7/x/"] $ \s ->
        (s, parseEpochPrefix cp (cp <> s)) `shouldBe` (s, Nothing)
    it "rejects different prefix" $
      parseEpochPrefix cp ("other/7/") `shouldBe` Nothing
