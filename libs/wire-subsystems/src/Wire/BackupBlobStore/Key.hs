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

-- | Pure S3 key layout of automatic backups:
-- @backup/<hmac(uid)>/<hmac(cid)>/<epoch>/<kind>/<id>@.
-- Layout is fixed forever once released.
module Wire.BackupBlobStore.Key
  ( BackupHmacKey,
    mkBackupHmacKey,
    BackupLocation (..),
    BackupPrefix (..),
    renderKey,
    renderPrefix,
    parseEpochPrefix,
    parseFileKey,
  )
where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (hmac)
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Id
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID qualified as UUID
import Imports
import Web.HttpApiData (parseUrlPiece, toUrlPiece)
import Wire.API.Backup

-- | At least 32 bytes of secret. 'Show' never reveals the bytes.
newtype BackupHmacKey = BackupHmacKey ByteString

instance Show BackupHmacKey where
  show _ = "<redacted>"

mkBackupHmacKey :: ByteString -> Either Text BackupHmacKey
mkBackupHmacKey bs
  | BS.length bs < 32 = Left "backup HMAC key must be at least 32 bytes"
  | otherwise = Right (BackupHmacKey bs)

data BackupLocation = BackupLocation
  { user :: UserId,
    client :: ClientId,
    epoch :: BackupEpoch,
    kind :: BackupKind,
    fileId :: BackupFileId
  }
  deriving (Eq, Ord, Show)

data BackupPrefix
  = UserPrefix UserId
  | ClientPrefix UserId ClientId
  | EpochPrefix UserId ClientId BackupEpoch
  deriving (Eq, Show)

-- | Lowercase hex of HMAC-SHA256 over the UTF-8 canonical text.
hmacHex :: BackupHmacKey -> Text -> Text
hmacHex (BackupHmacKey k) x =
  decodeUtf8 . B16.encode . convert $ hmac @ByteString @ByteString @SHA256 k (encodeUtf8 x)

userSeg :: BackupHmacKey -> UserId -> Text
userSeg k = hmacHex k . idToText

clientSeg :: BackupHmacKey -> ClientId -> Text
clientSeg k = hmacHex k . clientToText

renderKind :: BackupKind -> Text
renderKind BackupFull = "full"
renderKind BackupIncremental = "incremental"

parseKind :: Text -> Maybe BackupKind
parseKind "full" = Just BackupFull
parseKind "incremental" = Just BackupIncremental
parseKind _ = Nothing

renderPrefix :: BackupHmacKey -> BackupPrefix -> Text
renderPrefix k = \case
  UserPrefix u -> "backup/" <> userSeg k u <> "/"
  ClientPrefix u c -> renderPrefix k (UserPrefix u) <> clientSeg k c <> "/"
  EpochPrefix u c e -> renderPrefix k (ClientPrefix u c) <> toUrlPiece e <> "/"

renderKey :: BackupHmacKey -> BackupLocation -> Text
renderKey k loc =
  renderPrefix k (EpochPrefix loc.user loc.client loc.epoch)
    <> renderKind loc.kind
    <> "/"
    <> UUID.toText (unBackupFileId loc.fileId)

-- | Parses a CommonPrefix returned for a 'ClientPrefix' listing, e.g. "<clientPrefix>7/".
parseEpochPrefix :: Text -> Text -> Maybe BackupEpoch
parseEpochPrefix clientPrefix cp = do
  rest <- T.stripPrefix clientPrefix cp
  seg <- T.stripSuffix "/" rest
  parseCanonicalEpoch seg

-- | Parses a key returned for an 'EpochPrefix' listing.
parseFileKey :: Text -> Text -> Maybe (BackupKind, BackupFileId)
parseFileKey epochPrefix key = do
  rest <- T.stripPrefix epochPrefix key
  case T.splitOn "/" rest of
    [kindSeg, idSeg] -> do
      kind <- parseKind kindSeg
      uuid <- UUID.fromText idSeg
      -- canonical lowercase text only
      guard (UUID.toText uuid == idSeg)
      pure (kind, BackupFileId uuid)
    _ -> Nothing

-- | Plain decimal, no padding, >= 1, fits 'Word32'.
parseCanonicalEpoch :: Text -> Maybe BackupEpoch
parseCanonicalEpoch seg = do
  e <- either (const Nothing) Just (parseUrlPiece seg)
  guard (toUrlPiece e == seg)
  pure e
