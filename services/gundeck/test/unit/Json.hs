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

module Json where

import Control.Lens (set, view)
import Data.Aeson
import Data.Aeson.KeyMap (fromList)
import Data.Id
import Data.List1
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.Internal.BulkPush
import Wire.API.Internal.Notification
import Wire.API.Push.V2

tests :: TestTree
tests =
  testGroup
    "JSON"
    [ testProperty "push/recipient" $
        forAll genRecipient serialiseOkProp,
      testGroup
        "BulkPush"
        [ testCase "PushTarget example" $ do
            let serialized = "{\"conn_id\":\"nJ\",\"user_id\":\"09178cd7-3190-45ec-95aa-695edbb03960\"}"
                typed = PushTarget (read "09178cd7-3190-45ec-95aa-695edbb03960") (ConnId "nJ")
            assertEqual "" (decode serialized) (Just typed),
          testCase "BulkPushRequest example" $ do
            let serialized = "{\"bulkpush_req\":[{\"notification\":{\"payload\":[{\"Rk\":\"o\"},{\"n\":\"uy\"}],\"transient\":true,\"id\":\"d8f6c42e-f8da-4e7b-99e7-db66eccf8da1\"},\"targets\":[{\"conn_id\":\"88\",\"user_id\":\"7d94d3f0-f853-41d3-bd25-eb17c8f72f6e\"},{\"conn_id\":\"v\",\"user_id\":\"10158f18-3350-41c5-9eb4-374dee978e05\"}]},{\"notification\":{\"payload\":[{}],\"transient\":false,\"id\":\"8d3111d1-d010-47e6-b5db-d81cfbe8b0d4\"},\"targets\":[{\"conn_id\":\"nJ\",\"user_id\":\"09178cd7-3190-45ec-95aa-695edbb03960\"}]}]}"
                typed = Just (BulkPushRequest {fromBulkPushRequest = [(Notification {ntfId = read "d8f6c42e-f8da-4e7b-99e7-db66eccf8da1", ntfTransient = True, ntfPayload = list1 (fromList [("Rk", String "o")]) [fromList [("n", String "uy")]]}, [PushTarget {ptUserId = read "7d94d3f0-f853-41d3-bd25-eb17c8f72f6e", ptConnId = ConnId {fromConnId = "88"}}, PushTarget {ptUserId = read "10158f18-3350-41c5-9eb4-374dee978e05", ptConnId = ConnId {fromConnId = "v"}}]), (Notification {ntfId = read "8d3111d1-d010-47e6-b5db-d81cfbe8b0d4", ntfTransient = False, ntfPayload = list1 (fromList []) []}, [PushTarget {ptUserId = read "09178cd7-3190-45ec-95aa-695edbb03960", ptConnId = ConnId {fromConnId = "nJ"}}])]})
            assertEqual "" (decode serialized) (Just typed),
          testCase "BulkPushResponse example" $ do
            let serialized = "{\"bulkpush_resp\":[{\"status\":\"push_status_gone\",\"notif_id\":\"f2c218cf-6399-47fb-8d7b-726ed599af91\",\"target\":{\"conn_id\":\"\",\"user_id\":\"5b099991-364a-425d-91af-9b8e51ac2956\"}},{\"status\":\"push_status_ok\",\"notif_id\":\"d8e8d19a-6788-4180-afcd-bf84395f4cf6\",\"target\":{\"conn_id\":\"Lf\",\"user_id\":\"cccc316f-eaad-4d55-9798-3fd8b431106e\"}}]}"
                typed = BulkPushResponse {fromBulkPushResponse = [(read "f2c218cf-6399-47fb-8d7b-726ed599af91", PushTarget {ptUserId = read "5b099991-364a-425d-91af-9b8e51ac2956", ptConnId = ConnId {fromConnId = ""}}, PushStatusGone), (read "d8e8d19a-6788-4180-afcd-bf84395f4cf6", PushTarget {ptUserId = read "cccc316f-eaad-4d55-9798-3fd8b431106e", ptConnId = ConnId {fromConnId = "Lf"}}, PushStatusOk)]}
            assertEqual "" (decode serialized) (Just typed)
        ],
      testProperty "BulkPushRequest roundtrip"
        . forAll genBulkPushRequest
        $ \req -> eitherDecode (encode req) == Right req,
      testProperty "BulkPushResponse roundtrip"
        . forAll genBulkPushResponse
        $ \resp -> eitherDecode (encode resp) == Right resp
    ]

serialiseOkProp :: Recipient -> Property
serialiseOkProp r =
  property $
    let r' = decode (encode r)
     in (view recipientId <$> r') == Just (view recipientId r)
          && (view recipientRoute <$> r') == Just (view recipientRoute r)
          && (view recipientClients <$> r') == Just (view recipientClients r)

-----------------------------------------------------------------------------
-- Randomness

-- TODO: not sure if these exist elsewhere?  or should be moved elsewhere?
-- TODO: genObject, genAlphaNum are not very exhaustive.

genRecipient :: Gen Recipient
genRecipient = do
  r <- recipient <$> arbitrary <*> elements [RouteAny, RouteDirect]
  c <- genRecipientClients
  pure $ r & set recipientClients c

genRecipientClients :: Gen RecipientClients
genRecipientClients =
  oneof
    [ pure RecipientClientsAll,
      RecipientClientsSome . List1 <$> arbitrary
    ]

genBulkPushRequest :: Gen BulkPushRequest
genBulkPushRequest =
  BulkPushRequest
    <$> shortListOf ((,) <$> genNotification <*> scale (`div` 3) (listOf genPushTarget))

genBulkPushResponse :: Gen BulkPushResponse
genBulkPushResponse =
  BulkPushResponse
    <$> shortListOf (scale (`div` 3) ((,,) <$> arbitrary <*> genPushTarget <*> elements [minBound ..]))

genNotification :: Gen Notification
genNotification = Notification <$> arbitrary <*> arbitrary <*> (list1 <$> genobj <*> listOf genobj)
  where
    genobj = scale (`div` 3) genObject

genPushTarget :: Gen PushTarget
genPushTarget = PushTarget <$> arbitrary <*> (ConnId <$> genAlphaNum)

genObject :: Gen Object
genObject = fromList <$> listOf ((,) <$> genAlphaNum <*> (String <$> genAlphaNum))

genAlphaNum :: (IsString s) => Gen s
genAlphaNum = fromString <$> listOf (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))

shortListOf :: Gen a -> Gen [a]
shortListOf gen = choose (0, 5) >>= (`vectorOf` gen)
