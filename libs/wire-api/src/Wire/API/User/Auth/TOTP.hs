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

-- | Wire types for TOTP (RFC 6238) second factors.
--
-- Three of the types here carry secrets: 'TOTPSecret', 'TOTPCode' and
-- 'RecoveryCode'.  Their constructors are not exported and their
-- 'Show' is redacting.  This does not protect these secrets from
-- ending up in log files by accident!  See
module Wire.API.User.Auth.TOTP
  ( -- * Credential ids
    TOTPCredentialId,

    -- * The shared secret
    TOTPSecret,
    totpSecretFromBytes,
    totpSecretToBase32,
    totpSecretFromBase32,

    -- * Codes
    TOTPCode,
    totpCodeFromText,
    unsafeRevealTOTPCode,
    RecoveryCode,
    recoveryCodeFromText,
    unsafeRevealRecoveryCode,
    SecondFactorCode (..),
    secondFactorCodeFromText,

    -- * Parameters
    TOTPHashAlgorithm (..),
    totpHashAlgorithmToUriText,

    -- * Payloads
    TOTPCredentialInfo (..),
    NewTOTPCredential (..),
    TOTPEnrolment (..),
    ActivateTOTPCredential (..),
    TOTPCredentialName (..),
    TOTPRecoveryCodes (..),
    ReauthBody (..),
    reauthBodyObjectSchema,

    -- * URI construction
    otpauthURI,
  )
where

import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.ByteArray.Encoding (Base (Base32), convertFromBase, convertToBase)
import Data.ByteString qualified as BS
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Misc (PlainTextPassword6)
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Network.HTTP.Types.URI (urlEncode)
import Test.QuickCheck (elements, vectorOf)
import Wire.Arbitrary

--------------------------------------------------------------------------------
-- TOTPSecret

-- | The shared secret of a TOTP credential, in its raw binary form.
--
-- On the wire this is /unpadded/ Base32 (RFC 4648), which is what authenticator
-- apps consume.  See 'totpSecretToBase32' / 'totpSecretFromBase32' for the
-- exact rules.
newtype TOTPSecret = TOTPSecret ByteString
  deriving (Eq, Ord)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPSecret

instance Show TOTPSecret where
  show _ = "TOTPSecret <redacted>"

-- | Wrap raw secret bytes.  No validation: the subsystem decides how long a
-- secret is, wire-api only has to be able to render and parse it.
totpSecretFromBytes :: ByteString -> TOTPSecret
totpSecretFromBytes = TOTPSecret

-- | Unpadded Base32 (RFC 4648), upper case.
--
-- @memory@'s 'convertToBase' pads with @=@; our wire format does not, because
-- that is what authenticator apps and @otpauth://@ URIs use.
totpSecretToBase32 :: TOTPSecret -> Text
totpSecretToBase32 (TOTPSecret bs) =
  Text.dropWhileEnd (== '=') . Text.decodeUtf8 $ convertToBase Base32 bs

-- | Parse unpadded Base32.
--
-- Two deliberate decisions, both tested:
--
-- * lower case is accepted (authenticator apps and QR scanners are
--   inconsistent about case, and Base32 has no lower-case alphabet to confuse
--   it with);
--
-- * input that /arrives/ padded is rejected, because our wire format is
--   unpadded and accepting both would mean two encodings of the same secret.
totpSecretFromBase32 :: Text -> Either String TOTPSecret
totpSecretFromBase32 txt
  | Text.null txt = Left "TOTP secret must not be empty"
  | Text.any (== '=') txt = Left "TOTP secret must be unpadded base32 (RFC 4648)"
  | otherwise = TOTPSecret <$> convertFromBase Base32 (Text.encodeUtf8 padded)
  where
    upper = Text.toUpper txt
    padded = upper <> Text.replicate ((8 - Text.length upper `rem` 8) `rem` 8) "="

instance ToSchema TOTPSecret where
  schema =
    totpSecretToBase32
      .= parsedTextWithDoc
        "The shared secret, unpadded base32 (RFC 4648).  Returned exactly once, \
        \at enrolment.  Lower case is accepted on input; padding is not."
        "TOTPSecret"
        totpSecretFromBase32
      & doc' . S.schema . S.example ?~ A.toJSON ("JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP" :: Text)

-- | 20 random bytes = 160 bits, which is exactly 32 base32 characters and
-- therefore needs no padding.  This is what clients expect.
instance Arbitrary TOTPSecret where
  arbitrary = TOTPSecret . BS.pack <$> vectorOf 20 arbitrary

--------------------------------------------------------------------------------
-- TOTPCode

-- | A code produced by an authenticator app.
--
-- The length is checked against 6..9 rather than against the configured digit
-- count, because wire-api cannot see brig's config.  The exact-length check
-- belongs to the verifier.
newtype TOTPCode = TOTPCode Text
  deriving (Eq, Ord)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPCode

instance Show TOTPCode where
  show _ = "TOTPCode <redacted>"

totpCodeFromText :: Text -> Either String TOTPCode
totpCodeFromText t
  | not (Text.all isDigit t) = Left "TOTP code must consist of ASCII digits only"
  | Text.length t < 6 || Text.length t > 9 = Left "TOTP code must be 6 to 9 digits long"
  | otherwise = Right (TOTPCode t)

unsafeRevealTOTPCode :: TOTPCode -> Text
unsafeRevealTOTPCode (TOTPCode t) = t

instance ToSchema TOTPCode where
  schema =
    unsafeRevealTOTPCode
      .= parsedTextWithDoc
        "A code from the authenticator app: 6 to 9 ASCII digits, leading zeros significant."
        "TOTPCode"
        totpCodeFromText
      & doc' . S.schema . S.example ?~ A.toJSON ("014921" :: Text)

instance Arbitrary TOTPCode where
  arbitrary = TOTPCode . Text.pack <$> vectorOf 6 (elements ['0' .. '9'])

--------------------------------------------------------------------------------
-- RecoveryCode

-- | A single-use recovery code, @xxxx-xxxx-xxxx-xxxx@ in Crockford base32.
newtype RecoveryCode = RecoveryCode Text
  deriving (Eq, Ord)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema RecoveryCode

instance Show RecoveryCode where
  show _ = "RecoveryCode <redacted>"

-- | Crockford base32: no @I@, @L@, @O@, @U@, so the alphabet has no character
-- pair a human can confuse when reading a code off a printout.
crockfordAlphabet :: [Char]
crockfordAlphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

-- | Lower case is normalised to upper case, matching 'totpSecretFromBase32'.
--
-- Crockford's spec additionally maps @I@/@L@ to @1@ and @O@ to @0@ on input.
-- We do not do that transliteration: it is only useful once codes are actually
-- generated and checked, and it belongs with that ticket.
recoveryCodeFromText :: Text -> Either String RecoveryCode
recoveryCodeFromText t
  | length groups /= 4 || any ((/= 4) . Text.length) groups =
      Left "recovery code must have the shape xxxx-xxxx-xxxx-xxxx"
  | not (all (Text.all (`elem` crockfordAlphabet)) groups) =
      Left "recovery code must be Crockford base32 (no I, L, O, U)"
  | otherwise = Right (RecoveryCode upper)
  where
    upper = Text.toUpper t
    groups = Text.splitOn "-" upper

unsafeRevealRecoveryCode :: RecoveryCode -> Text
unsafeRevealRecoveryCode (RecoveryCode t) = t

instance ToSchema RecoveryCode where
  schema =
    unsafeRevealRecoveryCode
      .= parsedTextWithDoc
        "A single-use recovery code, `xxxx-xxxx-xxxx-xxxx` in Crockford base32 \
        \(alphabet `0123456789ABCDEFGHJKMNPQRSTVWXYZ`, i.e. no I, L, O or U).  \
        \Lower case is accepted on input."
        "RecoveryCode"
        recoveryCodeFromText
      & doc' . S.schema . S.example ?~ A.toJSON ("3MK7-9WQX-0V2H-8TZB" :: Text)

instance Arbitrary RecoveryCode where
  arbitrary =
    RecoveryCode . Text.intercalate "-"
      <$> replicateM 4 (Text.pack <$> vectorOf 4 (elements crockfordAlphabet))

--------------------------------------------------------------------------------
-- SecondFactorCode

-- | Wherever a second factor is /submitted/, any kind of code is
-- allowed.  Currently: only TOTP and recovery, but there may be
-- others in the future (yubikey, passkey, ...)
data SecondFactorCode
  = SFTotp TOTPCode
  | SFRecovery RecoveryCode
  deriving (Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform SecondFactorCode)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema SecondFactorCode

instance Show SecondFactorCode where
  show _ = "SecondFactorCode <redacted>"

secondFactorCodeFromText :: Text -> Either String SecondFactorCode
secondFactorCodeFromText t =
  case Text.stripPrefix "totp:" t of
    Just rest -> SFTotp <$> totpCodeFromText rest
    Nothing ->
      case Text.stripPrefix "recovery:" t of
        Just rest -> SFRecovery <$> recoveryCodeFromText rest
        Nothing -> Left $ "unknown second factor type in " <> show t

instance ToSchema SecondFactorCode where
  schema =
    secondFactorCodeToText
      .= parsedTextWithDoc
        "A second-factor code.  Each type uses a text prefix:\
        \<ul>\
        \<li><code>totp:...</code> — a code from an authenticator app (6-9 digits)</li>\
        \<li><code>recovery:...</code> — a recovery code (<code>xxxx-xxxx-xxxx-xxxx</code>)</li>\
        \</ul>"
        "SecondFactorCode"
        secondFactorCodeFromText
      & doc' . S.schema . S.example ?~ A.toJSON ("totp:014921" :: Text)
    where
      secondFactorCodeToText :: SecondFactorCode -> Text
      secondFactorCodeToText = \case
        SFTotp c -> "totp:" <> unsafeRevealTOTPCode c
        SFRecovery c -> "recovery:" <> unsafeRevealRecoveryCode c

--------------------------------------------------------------------------------
-- TOTPHashAlgorithm

-- | The HMAC hash a credential is driven with.  Site-configurable; stored per
-- credential, because an authenticator app has it baked in and cannot be
-- updated.
--
-- Note there are two renderings: JSON (and config) use lower case, the
-- @otpauth://@ URI uses upper case.  See 'totpHashAlgorithmToUriText'.
data TOTPHashAlgorithm
  = TOTPSHA1
  | TOTPSHA256
  | TOTPSHA512
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform TOTPHashAlgorithm)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPHashAlgorithm

instance ToSchema TOTPHashAlgorithm where
  schema =
    enum @Text $
      mconcat
        [ element "sha1" TOTPSHA1,
          element "sha256" TOTPSHA256,
          element "sha512" TOTPSHA512
        ]

-- | The spelling used in the @algorithm@ parameter of an @otpauth://@ URI.
totpHashAlgorithmToUriText :: TOTPHashAlgorithm -> Text
totpHashAlgorithmToUriText = \case
  TOTPSHA1 -> "SHA1"
  TOTPSHA256 -> "SHA256"
  TOTPSHA512 -> "SHA512"

--------------------------------------------------------------------------------
-- ReauthBody

-- | The re-authentication payload shared by every privileged endpoint.
data ReauthBody = ReauthBody
  { password :: Maybe PlainTextPassword6,
    totpCode :: Maybe SecondFactorCode
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ReauthBody)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ReauthBody

reauthBodyObjectSchema :: ObjectSchema SwaggerDoc ReauthBody
reauthBodyObjectSchema =
  ReauthBody
    <$> (.password) .= maybe_ (optField "password" schema)
    <*> (.totpCode) .= maybe_ (optField "totp_code" schema)

instance ToSchema ReauthBody where
  schema = object reauthBodyObjectSchema

--------------------------------------------------------------------------------
-- Payloads

-- | Public metadata about an enrolled credential.  The secret is
-- never part of this: it is returned exactly once, by enrolment.
-- There is deliberately no @last_used_at@ field, this is data we do
-- not want to track for privacy reasons.
data TOTPCredentialInfo = TOTPCredentialInfo
  { id_ :: TOTPCredentialId,
    name :: Text,
    createdAt :: UTCTimeMillis,
    activatedAt :: Maybe UTCTimeMillis
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TOTPCredentialInfo)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPCredentialInfo

instance ToSchema TOTPCredentialInfo where
  schema =
    object $
      TOTPCredentialInfo
        <$> (.id_)
          .= fieldWithDocModifier
            "id"
            (description ?~ "Identifies this credential in the `/self/totp/{id}` routes.")
            schema
        <*> (.name)
          .= fieldWithDocModifier
            "name"
            (description ?~ "The label the user gave this authenticator, e.g. \"work phone\".")
            schema
        <*> (.createdAt)
          .= fieldWithDocModifier
            "created_at"
            (description ?~ "When enrolment was started.")
            schema
        <*> (.activatedAt)
          .= maybe_
            ( optFieldWithDocModifier
                "activated_at"
                (description ?~ "When the credential was activated.  `null` means enrolment was never finished; such a credential cannot be used and expires.")
                schema
            )

-- | Body of @POST \/self\/totp@.  Embeds 'ReauthBody' flat.
data NewTOTPCredential = NewTOTPCredential
  { name :: Text,
    clientTime :: UTCTimeMillis,
    reauth :: ReauthBody
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewTOTPCredential)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema NewTOTPCredential

instance ToSchema NewTOTPCredential where
  schema =
    object $
      NewTOTPCredential
        <$> (.name)
          .= fieldWithDocModifier
            "name"
            (description ?~ "A label for this authenticator, shown in `GET /self/totp`.")
            schema
        <*> (.clientTime)
          .= fieldWithDocModifier
            "client_time"
            (description ?~ "The client's idea of \"now\".  Used to detect a mis-set device clock before it turns into a mysterious \"invalid code\" at login.")
            schema
        <*> (.reauth) .= reauthBodyObjectSchema

-- | The one-time response to @POST \/self\/totp@.
data TOTPEnrolment = TOTPEnrolment
  { credentialId :: TOTPCredentialId,
    secret :: TOTPSecret,
    uri :: Text,
    algorithm :: TOTPHashAlgorithm,
    digits :: Word8,
    period :: Word16,
    serverTime :: UTCTimeMillis,
    expiresAt :: UTCTimeMillis
  }
  deriving (Eq, Generic)
  deriving (Arbitrary) via (GenericUniform TOTPEnrolment)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPEnrolment

instance Show TOTPEnrolment where
  show _ = "TOTPEnrolment <redacted>"

instance ToSchema TOTPEnrolment where
  schema =
    object $
      TOTPEnrolment
        <$> (.credentialId)
          .= fieldWithDocModifier
            "credential_id"
            (description ?~ "Pass this to `POST /self/totp/{id}/activate` to finish enrolment.")
            schema
        <*> (.secret)
          .= fieldWithDocModifier
            "secret"
            (description ?~ "The shared secret, unpadded base32.  Returned **exactly once**; `GET /self/totp` never returns it again.  Also present in `uri`.")
            schema
        <*> (.uri)
          .= fieldWithDocModifier
            "uri"
            (description ?~ otpauthUriDesc)
            schema
        <*> (.algorithm)
          .= fieldWithDocModifier
            "algorithm"
            (description ?~ "The HMAC hash this credential uses.  Note most authenticator apps ignore this and assume `sha1`.")
            schema
        <*> (.digits)
          .= fieldWithDocModifier
            "digits"
            (description ?~ "How many digits a code of this credential has.")
            schema
        <*> (.period)
          .= fieldWithDocModifier
            "period"
            (description ?~ "The time step in seconds.")
            schema
        <*> (.serverTime)
          .= fieldWithDocModifier
            "server_time"
            (description ?~ "The server's idea of \"now\", so a client can self-diagnose clock drift before even trying to activate.")
            schema
        <*> (.expiresAt)
          .= fieldWithDocModifier
            "expires_at"
            (description ?~ "When this unfinished enrolment expires.  Activating after that fails with `totp-enrolment-expired` and the whole flow has to start over.")
            schema

otpauthUriDesc :: Text
otpauthUriDesc =
  "The `otpauth://` URI to encode into a QR code.  **The client renders the QR \
  \code**; the server returns no image.  Format:\n\n\
  \`otpauth://totp/{issuer}:{label}?secret={secret}&issuer={issuer}&algorithm={algorithm}&digits={digits}&period={period}`\n\n\
  \where `label` identifies the account inside the app (percent-encoded), \
  \`secret` is the same value as the `secret` field of this response, \
  \`algorithm` is the upper-case spelling (`SHA1`, `SHA256`, `SHA512`) of the \
  \`algorithm` field, and `digits` and `period` are those fields.  A client that \
  \does not want to parse this can build its own entry from the sibling fields."

-- | Body of @POST \/self\/totp\/{id}\/activate@.
data ActivateTOTPCredential = ActivateTOTPCredential
  { code :: TOTPCode,
    clientTime :: UTCTimeMillis
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ActivateTOTPCredential)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema ActivateTOTPCredential

instance ToSchema ActivateTOTPCredential where
  schema =
    object $
      ActivateTOTPCredential
        <$> (.code)
          .= fieldWithDocModifier
            "code"
            (description ?~ "A code read off the authenticator app that just scanned the enrolment QR code.  This is what proves the app got the secret.")
            schema
        <*> (.clientTime)
          .= fieldWithDocModifier
            "client_time"
            (description ?~ "The client's idea of \"now\"; see `POST /self/totp`.")
            schema

-- | Body of @PUT \/self\/totp\/{id}@ request.
newtype TOTPCredentialName = TOTPCredentialName {name :: Text}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TOTPCredentialName)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPCredentialName

instance ToSchema TOTPCredentialName where
  schema =
    object $
      TOTPCredentialName
        <$> (.name)
          .= fieldWithDocModifier
            "name"
            (description ?~ "The new label for this authenticator.")
            schema

-- | Response to @POST \/self\/totp\/{id}\/activate@.
newtype TOTPRecoveryCodes = TOTPRecoveryCodes {codes :: [RecoveryCode]}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TOTPRecoveryCodes)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema TOTPRecoveryCodes

instance ToSchema TOTPRecoveryCodes where
  schema =
    object $
      TOTPRecoveryCodes
        <$> (.codes)
          .= fieldWithDocModifier
            "codes"
            (description ?~ "Single-use codes that stand in for an authenticator the user no longer has.  Shown **once**; there is no endpoint that returns them again.")
            (array schema)

----------------------------------------------------------------
-- URI construction

-- | Build an @otpauth:\/\/@ URI for a TOTP credential.  The format is
-- documented on the @uri@ field of 'TOTPEnrolment'.
--
-- See the Haddock on 'totpHashAlgorithmToUriText' for the algorithm spelling.
otpauthURI ::
  -- | Label displayed in the authenticator app (percent-encoded).
  Text ->
  -- | The shared secret.
  TOTPSecret ->
  -- | Hash algorithm ('totpHashAlgorithmToUriText').
  TOTPHashAlgorithm ->
  -- | Number of digits.
  Word8 ->
  -- | Time step in seconds.
  Word16 ->
  -- | Issuer name.
  Text ->
  Text
otpauthURI label secret algorithm digits period issuer =
  mconcat
    [ "otpauth://totp/",
      issuer,
      ":",
      percentEncode label,
      "?secret=",
      totpSecretToBase32 secret,
      "&issuer=",
      percentEncode issuer,
      "&algorithm=",
      totpHashAlgorithmToUriText algorithm,
      "&digits=",
      Text.pack (show digits),
      "&period=",
      Text.pack (show period)
    ]
  where
    percentEncode :: Text -> Text
    percentEncode = Text.decodeUtf8 . urlEncode True . Text.encodeUtf8
