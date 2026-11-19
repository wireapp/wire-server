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
-- @backup/<hmac(uid)>/<hmac(cid)>/<epoch>/<fullId>/full@ and
-- @backup/<hmac(uid)>/<hmac(cid)>/<epoch>/<fullId>/incremental/<id>@.
-- Layout is fixed forever once released.
module Wire.BackupBlobStore.Key
  ( BackupHmacKey,
    mkBackupHmacKey,
    BackupFileRef (..),
    fileRefId,
    BackupLocation (..),
    BackupPrefix (..),
    renderKey,
    renderPrefix,
    renderTombstonePrefix,
    renderTombstoneKey,
    parseTombstoneKey,
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

-- | A file of an epoch. An incremental file is stored under the full file it extends.
data BackupFileRef
  = FullFile BackupFileId
  | -- | The full file it extends, its own id
    IncrementalFile BackupFileId BackupFileId
  deriving (Eq, Ord, Show)

-- | The id of the file itself.
fileRefId :: BackupFileRef -> BackupFileId
fileRefId (FullFile i) = i
fileRefId (IncrementalFile _ i) = i

data BackupLocation = BackupLocation
  { user :: UserId,
    client :: ClientId,
    epoch :: BackupEpoch,
    file :: BackupFileRef
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

renderPrefix :: BackupHmacKey -> BackupPrefix -> Text
renderPrefix k = \case
  UserPrefix u -> "backup/" <> userSeg k u <> "/"
  ClientPrefix u c -> renderPrefix k (UserPrefix u) <> clientSeg k c <> "/"
  EpochPrefix u c e -> renderPrefix k (ClientPrefix u c) <> toUrlPiece e <> "/"

renderKey :: BackupHmacKey -> BackupLocation -> Text
renderKey k loc =
  renderPrefix k (EpochPrefix loc.user loc.client loc.epoch) <> case loc.file of
    FullFile f -> uuidText f <> "/full"
    IncrementalFile f i -> uuidText f <> "/incremental/" <> uuidText i

uuidText :: BackupFileId -> Text
uuidText = UUID.toText . unBackupFileId

-- | Tombstones of a client, written by its backup deletion: @<clientPrefix>.tombstones/<id>@.
-- Not an epoch, so listings of epochs ignore them.
renderTombstonePrefix :: BackupHmacKey -> UserId -> ClientId -> Text
renderTombstonePrefix k u c = renderPrefix k (ClientPrefix u c) <> ".tombstones/"

renderTombstoneKey :: BackupHmacKey -> UserId -> ClientId -> BackupFileId -> Text
renderTombstoneKey k u c i = renderTombstonePrefix k u c <> uuidText i

-- | Parses a key returned for a 'renderTombstonePrefix' listing.
parseTombstoneKey :: Text -> Text -> Maybe BackupFileId
parseTombstoneKey prefix key = T.stripPrefix prefix key >>= parseCanonicalId

-- | Parses a CommonPrefix returned for a 'ClientPrefix' listing, e.g. "<clientPrefix>7/".
parseEpochPrefix :: Text -> Text -> Maybe BackupEpoch
parseEpochPrefix clientPrefix cp = do
  rest <- T.stripPrefix clientPrefix cp
  seg <- T.stripSuffix "/" rest
  parseCanonicalEpoch seg

-- | Parses a key returned for an 'EpochPrefix' listing.
parseFileKey :: Text -> Text -> Maybe BackupFileRef
parseFileKey epochPrefix key = do
  rest <- T.stripPrefix epochPrefix key
  case T.splitOn "/" rest of
    [f, "full"] -> FullFile <$> parseId f
    [f, "incremental", i] -> IncrementalFile <$> parseId f <*> parseId i
    _ -> Nothing
  where
    parseId = parseCanonicalId

-- | Canonical lowercase text only.
parseCanonicalId :: Text -> Maybe BackupFileId
parseCanonicalId t = do
  uuid <- UUID.fromText t
  guard (UUID.toText uuid == t)
  pure (BackupFileId uuid)

-- | Plain decimal, no padding, >= 1, fits 'Word32'.
parseCanonicalEpoch :: Text -> Maybe BackupEpoch
parseCanonicalEpoch seg = do
  e <- either (const Nothing) Just (parseUrlPiece seg)
  guard (toUrlPiece e == seg)
  pure e
