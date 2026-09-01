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

-- | Unit tests for "Gundeck.Push.Web.Serialise".
module WebPushSerialise
  ( tests,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Id (UserId)
import Gundeck.Push.Native.Types (NativePush (..))
import Gundeck.Push.Web.Crypto (maxPlaintextLength)
import Gundeck.Push.Web.Serialise
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Notification (mkNotificationId)
import Wire.API.Push.V2 (Priority (HighPriority))

tests :: TestTree
tests =
  testGroup
    "WebPushSerialise"
    [ testCase "produces {type:'notice', data:{id}, user} shape" $ do
        nid <- mkNotificationId
        let np = NativePush nid HighPriority Nothing
            uid = read "00000000-0000-0001-0000-000000000000" :: UserId
        case serialise np uid of
          Left err -> assertFailure ("serialise failed: " <> show err)
          Right json -> do
            v <- decodeObject json
            KeyMap.lookup (Key.fromText "type") v @?= Just (String "notice")
            case KeyMap.lookup (Key.fromText "data") v of
              Just (Object dataObj) ->
                -- 'id' must be present; its exact value follows the
                -- 'NotificationId' ToJSON instance (tested elsewhere).
                assertBool "data.id key missing" (KeyMap.member (Key.fromText "id") dataObj)
              other -> assertFailure ("expected data object, got " <> show other)
            assertBool "user key missing" (KeyMap.member (Key.fromText "user") v),
      testCase "roundtrips through Aeson decode (is valid JSON)" $ do
        nid <- mkNotificationId
        let np = NativePush nid HighPriority Nothing
            uid = read "00000000-0000-0001-0000-000000000001" :: UserId
        case serialise np uid of
          Right json ->
            case Aeson.decode (LBS.fromStrict json) :: Maybe Value of
              Just _ -> pure ()
              Nothing -> assertFailure "serialise produced invalid JSON"
          Left err -> assertFailure ("serialise failed: " <> show err),
      testCase "payload is well under the RFC 8291 §4 limit" $ do
        nid <- mkNotificationId
        let np = NativePush nid HighPriority Nothing
            uid = read "00000000-0000-0001-0000-000000000002" :: UserId
        case serialise np uid of
          Right json ->
            assertBool
              ("payload too large: " <> show (BS.length json))
              (BS.length json <= maxPlaintextLength)
          Left err -> assertFailure ("serialise failed: " <> show err)
    ]

decodeObject :: ByteString -> IO (KeyMap.KeyMap Value)
decodeObject bs =
  case Aeson.decode (LBS.fromStrict bs) of
    Just (Object o) -> pure o
    _ -> assertFailure "expected JSON object"
