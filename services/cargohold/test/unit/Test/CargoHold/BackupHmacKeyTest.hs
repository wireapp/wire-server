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

-- | Loading of the @backupHmacKey@ secret file.
module Test.CargoHold.BackupHmacKeyTest (tests) where

import CargoHold.App (loadBackupHmacKey)
import Control.Exception (ErrorCall (..), try)
import qualified Data.ByteString as BS
import Data.Id (UserId)
import Imports
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Temporary (withSystemTempDirectory)
import Util.Options (FilePathSecrets (..))
import Wire.BackupBlobStore.Key

tests :: TestTree
tests =
  testGroup
    "BackupHmacKey"
    [ testCase "one trailing newline is stripped" $ do
        uid <- generate arbitrary
        withNewline <- loadBytes (secret32 <> "\n")
        plain <- loadBytes secret32
        prefixOf uid withNewline @?= prefixOf uid plain,
      testCase "short key is rejected without revealing it" $ do
        -- 31 bytes plus newline: the newline does not count
        res <- try @ErrorCall (loadBytes (BS.take 31 secret32 <> "\n"))
        case res of
          Right _ -> assertFailure "short key accepted"
          Left (ErrorCall m) -> do
            assertBool "names the option" ("backupHmacKey" `isInfixOf` m)
            assertBool "no content" (not ("0123456789" `isInfixOf` m))
    ]

secret32 :: ByteString
secret32 = "0123456789abcdef0123456789abcdef"

loadBytes :: ByteString -> IO BackupHmacKey
loadBytes bs = withSystemTempDirectory "backup-hmac" $ \dir -> do
  let path = dir <> "/key"
  BS.writeFile path bs
  loadBackupHmacKey (FilePathSecrets path)

prefixOf :: UserId -> BackupHmacKey -> Text
prefixOf uid k = renderPrefix k (UserPrefix uid)
