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

module Test.Wire.API.Golden.Manual.TOTP where

import Data.Id
import Data.Json.Util
import Data.Misc (plainTextPassword6Unsafe)
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.User.Auth.TOTP

totpCredentialId1, totpCredentialId2 :: TOTPCredentialId
totpCredentialId1 = Id (fromJust (UUID.fromString "0f2e1b9c-6c6c-4a0e-9a3f-17a2cf15a001"))
totpCredentialId2 = Id (fromJust (UUID.fromString "7a63cf28-13f8-4f2e-8b5d-2d4c9f0b1002"))

createdAt1, activatedAt1, expiresAt1 :: UTCTimeMillis
createdAt1 = fromJust (readUTCTimeMillis "2026-02-03T11:17:05.021Z")
activatedAt1 = fromJust (readUTCTimeMillis "2026-02-03T11:18:44.903Z")
expiresAt1 = fromJust (readUTCTimeMillis "2026-02-03T11:32:05.021Z")

-- | @Hello!\\222\\173\\190\\239@, the RFC 4648 section 10 base32 test vector,
-- which encodes to @JBSWY3DPEHPK3PXP@ without padding.
secret1 :: TOTPSecret
secret1 = totpSecretFromBytes "Hello!\222\173\190\239"

totpCode1 :: TOTPCode
totpCode1 = either (error . show) id (totpCodeFromText "014921")

recoveryCode1, recoveryCode2 :: RecoveryCode
recoveryCode1 = either (error . show) id (recoveryCodeFromText "3MK7-9WQX-0V2H-8TZB")
recoveryCode2 = either (error . show) id (recoveryCodeFromText "P0RT-4N5Y-WDXG-6JEC")

testObject_TOTPEnrolment_1 :: TOTPEnrolment
testObject_TOTPEnrolment_1 =
  TOTPEnrolment
    { credentialId = totpCredentialId1,
      secret = secret1,
      uri = "otpauth://totp/Wire:alice%40example.com?secret=JBSWY3DPEHPK3PXP&issuer=Wire&algorithm=SHA1&digits=6&period=30",
      algorithm = TOTPSHA1,
      digits = 6,
      period = 30,
      serverTime = createdAt1,
      expiresAt = expiresAt1
    }

-- | A non-default parameter set, so the golden file pins down that
-- @algorithm@ renders lower case in JSON even though the URI spells it upper
-- case.
testObject_TOTPEnrolment_2 :: TOTPEnrolment
testObject_TOTPEnrolment_2 =
  TOTPEnrolment
    { credentialId = totpCredentialId2,
      secret = secret1,
      uri = "otpauth://totp/Wire:0f2e1b9c-6c6c-4a0e-9a3f-17a2cf15a001?secret=JBSWY3DPEHPK3PXP&issuer=Wire&algorithm=SHA512&digits=8&period=60",
      algorithm = TOTPSHA512,
      digits = 8,
      period = 60,
      serverTime = createdAt1,
      expiresAt = expiresAt1
    }

-- | Activated.
testObject_TOTPCredentialInfo_1 :: TOTPCredentialInfo
testObject_TOTPCredentialInfo_1 =
  TOTPCredentialInfo
    { id_ = totpCredentialId1,
      name = "work phone",
      createdAt = createdAt1,
      activatedAt = Just activatedAt1
    }

-- | Pending: @activated_at@ is @null@, not absent.
testObject_TOTPCredentialInfo_2 :: TOTPCredentialInfo
testObject_TOTPCredentialInfo_2 =
  TOTPCredentialInfo
    { id_ = totpCredentialId2,
      name = "\8364 laptop \128273",
      createdAt = createdAt1,
      activatedAt = Nothing
    }

testObject_TOTPRecoveryCodes_1 :: TOTPRecoveryCodes
testObject_TOTPRecoveryCodes_1 = TOTPRecoveryCodes [recoveryCode1, recoveryCode2]

testObject_TOTPRecoveryCodes_2 :: TOTPRecoveryCodes
testObject_TOTPRecoveryCodes_2 = TOTPRecoveryCodes []

-- | Password only, which is what a user without a second factor sends.
testObject_ReauthBody_1 :: ReauthBody
testObject_ReauthBody_1 =
  ReauthBody
    { password = Just (plainTextPassword6Unsafe "secret password"),
      totpCode = Nothing
    }

-- | Password plus a code from an authenticator app.
testObject_ReauthBody_2 :: ReauthBody
testObject_ReauthBody_2 =
  ReauthBody
    { password = Just (plainTextPassword6Unsafe "secret password"),
      totpCode = Just (SFTotp totpCode1)
    }

-- | Password plus a recovery code: the other branch of 'SecondFactorCode',
-- which renders into the same @totp_code@ field.
testObject_ReauthBody_3 :: ReauthBody
testObject_ReauthBody_3 =
  ReauthBody
    { password = Nothing,
      totpCode = Just (SFRecovery recoveryCode1)
    }

-- | SSO auth (not sure this is a thing, but its aeson representation is valid).
testObject_ReauthBody_4 :: ReauthBody
testObject_ReauthBody_4 =
  ReauthBody
    { password = Nothing,
      totpCode = Nothing
    }

-- | The flattened 'ReauthBody': one request, one body, no nested @reauth@ key.
testObject_NewTOTPCredential_1 :: NewTOTPCredential
testObject_NewTOTPCredential_1 =
  NewTOTPCredential
    { name = "work phone",
      clientTime = createdAt1,
      reauth = testObject_ReauthBody_1
    }

testObject_ActivateTOTPCredential_1 :: ActivateTOTPCredential
testObject_ActivateTOTPCredential_1 =
  ActivateTOTPCredential
    { code = totpCode1,
      clientTime = activatedAt1
    }

testObject_TOTPCredentialName_1 :: TOTPCredentialName
testObject_TOTPCredentialName_1 = TOTPCredentialName "private phone"
