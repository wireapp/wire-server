{-# LANGUAGE OverloadedStrings #-}

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

module Test.Wire.API.Golden.Generated.WebPushSubscription_user where

import Data.ByteString.Char8 qualified as BC8
import Data.Id (ClientId (..))
import Imports (Maybe (Just, Nothing))
import Wire.API.Push.V2.WebSubscription
  ( AuthSecret (..),
    EndpointUrl (..),
    P256dhKey (..),
    WebPushKeys (..),
    WebPushSubscription,
    webPushSubscription,
  )

-- | 0x04 (uncompressed point marker, RFC 8291) followed by 64 zero bytes.
testP256dh :: P256dhKey
testP256dh = P256dhKey (BC8.cons '\x04' (BC8.replicate 64 '\x00'))

testObject_WebPushSubscription_user_1 :: WebPushSubscription
testObject_WebPushSubscription_user_1 =
  webPushSubscription
    (EndpointUrl "https://fcm.googleapis.com/fcm/send/cid")
    (WebPushKeys testP256dh (AuthSecret (BC8.replicate 16 '\xA0')))
    (Just 1750291200000)
    (ClientId 0x17)

testObject_WebPushSubscription_user_2 :: WebPushSubscription
testObject_WebPushSubscription_user_2 =
  webPushSubscription
    (EndpointUrl "https://example.com/webpush/sub/2")
    (WebPushKeys testP256dh (AuthSecret (BC8.replicate 16 '\x07')))
    Nothing
    (ClientId 0x2)
