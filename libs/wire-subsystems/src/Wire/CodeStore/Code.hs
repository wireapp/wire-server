{-# LANGUAGE StrictData #-}

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

module Wire.CodeStore.Code
  ( Code (..),
    CodeReferent (..),
    CodeTarget (..),
    codeConvId,
    codeTarget,
    codeReferentFromTarget,
    toCode,
    generate,
    mkKey,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.Code
import Data.Id
import Data.Range
import Data.Text.Ascii qualified as Ascii
import Data.UUID (UUID)
import Imports
import OpenSSL.EVP.Digest (digestBS, getDigestByName)
import OpenSSL.Random (randBytes)
import Wire.API.Password (Password)
import Wire.API.PostgresMarshall

-- | The conversation or meeting a 'Code' refers to. Since both 'ConvId' and
-- 'MeetingId' are 'Id' values over a 'UUID', they are stored in a single uuid
-- column on the database, disambiguated by this constructor.
data CodeReferent
  = CodeReferentConv ConvId
  | CodeReferentMeeting MeetingId
  deriving (Eq, Show, Generic)

-- | Database discriminator for 'CodeReferent'. Stored in the @target@ column
-- of @conversation_codes@ to distinguish conversation codes from meeting codes.
data CodeTarget = CodeTargetConv | CodeTargetMeeting
  deriving (Eq, Show, Generic)

codeTarget :: CodeReferent -> CodeTarget
codeTarget CodeReferentConv {} = CodeTargetConv
codeTarget CodeReferentMeeting {} = CodeTargetMeeting

codeReferentFromTarget :: CodeTarget -> UUID -> CodeReferent
codeReferentFromTarget CodeTargetConv uid = CodeReferentConv (Id uid)
codeReferentFromTarget CodeTargetMeeting uid = CodeReferentMeeting (Id uid)

instance PostgresMarshall Text CodeTarget where
  postgresMarshall CodeTargetConv = "conv"
  postgresMarshall CodeTargetMeeting = "meeting"

instance PostgresUnmarshall Text CodeTarget where
  postgresUnmarshall = \case
    "conv" -> Right CodeTargetConv
    "meeting" -> Right CodeTargetMeeting
    other -> Left $ "unexpected code target: " <> other

data Code = Code
  { codeKey :: !Key,
    codeValue :: !Value,
    codeTTL :: !Timeout,
    codeReferent :: !CodeReferent,
    codeHasPassword :: !Bool
  }
  deriving (Eq, Show, Generic)

toCode :: Key -> (Value, Int32, CodeReferent, Maybe Password) -> (Code, Maybe Password)
toCode k (val, ttl, ref, mPw) =
  ( Code
      { codeKey = k,
        codeValue = val,
        codeTTL = Timeout (fromIntegral ttl),
        codeReferent = ref,
        codeHasPassword = isJust mPw
      },
    mPw
  )

-- | Extract the 'ConvId' from a 'Code' that refers to a conversation.
-- Returns 'Nothing' for codes that refer to a meeting.
codeConvId :: Code -> Maybe ConvId
codeConvId c = case codeReferent c of
  CodeReferentConv cid -> Just cid
  CodeReferentMeeting _ -> Nothing

-- Note on key/value used for a conversation Code
--
-- For similar reasons to those given for Codes used for verification, Password reset, etc
-- (see services/brig/src/Brig/Code.hs Note [Unique keys])
-- The 'key' is a stable, truncated, base64 encoded sha256 hash of the conversation ID
-- The 'value' is a base64 encoded, 120-bit random value (changing on each generation)

generate :: (MonadIO m) => CodeReferent -> Timeout -> m Code
generate ref t = do
  key <- mkKey ref
  val <- liftIO $ Value . unsafeRange . Ascii.encodeBase64Url <$> randBytes 15
  pure
    Code
      { codeKey = key,
        codeValue = val,
        codeReferent = ref,
        codeTTL = t,
        codeHasPassword = False
      }

mkKey :: (MonadIO m) => CodeReferent -> m Key
mkKey (CodeReferentConv cid) = mkKeyId cid
mkKey (CodeReferentMeeting mid) = mkKeyId mid

mkKeyId :: (MonadIO m) => Id a -> m Key
mkKeyId ident = do
  sha256 <- liftIO $ fromJust <$> getDigestByName "SHA256"
  pure $ Key . unsafeRange . Ascii.encodeBase64Url . BS.take 15 $ digestBS sha256 (toByteString' ident)
