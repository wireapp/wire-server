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

module Test.Wire.API.MLS where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation

tests :: TestTree
tests =
  testGroup "MLS" $
    [ testCase "parse key packages" testParseKeyPackage
    ]

testParseKeyPackage :: IO ()
testParseKeyPackage = do
  kpData <- LBS.readFile "test/resources/key_package1.mls"
  case decodeMLS @KeyPackage kpData of
    Left err -> assertFailure (T.unpack err)
    Right (kpTBS -> kp) -> do
      kpProtocolVersion kp @?= ProtocolMLS
      kpCipherSuite kp @?= CipherSuite 1
      BS.length (kpInitKey kp) @?= 32
      case decodeMLS' @ClientIdentity (bcIdentity (kpCredential kp)) of
        Left err -> assertFailure $ "Failed to parse identity: " <> T.unpack err
        Right identity ->
          identity
            @?= ClientIdentity
              { ciDomain = Domain "mls.example.com",
                ciUser = Id (fromJust (UUID.fromString "b455a431-9db6-4404-86e7-6a3ebe73fcaf")),
                ciClient = newClientId 0x3ae58155
              }
