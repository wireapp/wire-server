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

-- | Plaintext notice JSON for web push (RFC 8030 \/ RFC 8291).
--
-- This is the 'Gundeck.Push.Native.Serialise'-equivalent for the web transport:
-- the same logical @{"type":"notice","data":{"id":...},"user":...}@ shape, but
-- produced as a strict 'ByteString' for 'Gundeck.Push.Web.Crypto.encryptPayload'
-- rather than wrapped in a transport-specific (GCM\/APNS) envelope for SNS.
-- The native push payload is encrypted and delivered by AWS SNS; for web push,
-- gundeck itself is the RFC 8030 application server and encrypts the body per
-- RFC 8291 before POSTing (see "Gundeck.Push.Web").
--
-- RFC 8291 §4 mandates a single aes128gcm record per message, which caps the
-- plaintext at 'maxPlaintextLength' (3993) bytes. 'serialise' enforces this
-- bound and refuses to emit an oversized payload: the browser push service
-- would reject the resulting body anyway, and surfacing the failure here lets
-- dispatch ('Gundeck.Push.Web.push1') record it on the @web_push_too_large@
-- counter instead of attempting a doomed HTTP POST.
module Gundeck.Push.Web.Serialise
  ( serialise,
    WebPushSerialiseError (..),
  )
where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Id (UserId)
import Gundeck.Push.Native.Types (NativePush (npNotificationid))
import Gundeck.Push.Web.Crypto (maxPlaintextLength)
import Imports

-- | Reasons 'serialise' can refuse to build a payload.
data WebPushSerialiseError
  = -- | The serialised JSON exceeds the RFC 8291 §4 single-record plaintext
    -- limit ('maxPlaintextLength', 3993 bytes). Encrypting it would yield a
    -- multi-record aes128gcm body, which browser push services reject.
    WebPushPayloadTooLarge
  deriving stock (Eq, Show)

-- | Build the plaintext notice JSON that 'Gundeck.Push.Web.Crypto.encryptPayload'
-- encrypts for a single web push subscription. The shape mirrors the native
-- notice so the in-app payload is identical across transports:
--
-- @{"type":"notice","data":{"id":"<notification-id>"},"user":"<user-id>"}@
--
-- Returns 'Left' 'WebPushPayloadTooLarge' when the serialised JSON would not
-- fit in a single RFC 8291 record. In practice the JSON is well under the
-- 3993-byte budget — it carries only ids — but the check is mandated by
-- RFC 8291 §4 and defends against a pathological (or attacker-crafted)
-- 'NotificationId'.
serialise :: NativePush -> UserId -> Either WebPushSerialiseError ByteString
serialise np uid
  | BS.length json > maxPlaintextLength = Left WebPushPayloadTooLarge
  | otherwise = Right json
  where
    json =
      LBS.toStrict $
        Aeson.encode $
          object
            [ "type" .= ("notice" :: Text),
              "data" .= object ["id" .= np.npNotificationid],
              "user" .= uid
            ]
