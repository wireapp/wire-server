{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
import Data.Misc (PlainTextPassword6, plainTextPassword6Unsafe, plainTextPassword8Unsafe)
import Data.Text.Ascii (validate)
import Data.UUID qualified as UUID (fromString)
import Imports
import Test.Wire.API.Golden.Generated.InvitationCode_user (testObject_InvitationCode_user_1)
import Test.Wire.API.Golden.Manual.App (testObject_NewApp_1)
import Wire.API.Password (PasswordReqBody (..))
import Wire.API.Team qualified as Team
import Wire.API.Team.Invitation (AcceptTeamInvitation (..))
import Wire.API.Team.LegalHold qualified as LegalHold
import Wire.API.Team.Member qualified as Member
import Wire.API.User qualified as User
import Wire.API.User.Auth.ReAuth (ReAuthUser (..))
import Wire.API.User.Auth.TOTP
import Wire.API.User.Client qualified as Client
import Wire.API.User.Client.Prekey (lastPrekey)
import Wire.API.User.Password qualified as Password
import Wire.API.User.Scim qualified as Scim

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

-- A populated second factor for every request body that accepts totp_code.
-- Keep the original fixtures too: they cover clients that omit the field.
secondFactor1 :: Maybe SecondFactorCode
secondFactor1 = Just (SFTotp totpCode1)

testObject_PasswordReqBody_totp_1 :: PasswordReqBody
testObject_PasswordReqBody_totp_1 = PasswordReqBody (Just password1) secondFactor1

testObject_PasswordChange_totp_1 :: User.PasswordChange
testObject_PasswordChange_totp_1 =
  User.PasswordChange (Just password1) (plainTextPassword8Unsafe "new-password") secondFactor1

testObject_EmailUpdate_totp_1 :: User.EmailUpdate
testObject_EmailUpdate_totp_1 =
  User.EmailUpdate (User.unsafeEmailAddress "alice" "example.com") secondFactor1

testObject_EmailUpdate_withoutTotpCode_1 :: NoTotpCode User.EmailUpdate
testObject_EmailUpdate_withoutTotpCode_1 =
  NoTotpCode (User.EmailUpdate (User.unsafeEmailAddress "alice" "example.com") Nothing)

testObject_EmailUpdate_withoutTotpCode_2 :: NoTotpCode User.EmailUpdate
testObject_EmailUpdate_withoutTotpCode_2 =
  NoTotpCode (User.EmailUpdate (User.unsafeEmailAddress "bob+updates" "example.org") Nothing)

-- Encoding must omit the second factor even when the wrapped value has one.
-- This is intentionally not a roundtrip: decoding always sets it to Nothing.
testObject_EmailUpdate_withoutTotpCode_populated_1 :: NoTotpCode User.EmailUpdate
testObject_EmailUpdate_withoutTotpCode_populated_1 = NoTotpCode testObject_EmailUpdate_totp_1

testObject_DeleteUser_totp_1 :: User.DeleteUser
testObject_DeleteUser_totp_1 = User.DeleteUser (Just password1) secondFactor1

testObject_NewApp_totp_1 :: User.NewApp
testObject_NewApp_totp_1 = testObject_NewApp_1 {User.totpCode = secondFactor1}

testObject_ReAuthUser_totp_1 :: ReAuthUser
testObject_ReAuthUser_totp_1 =
  ReAuthUser (Just password1) Nothing (Just User.DeleteTeam) secondFactor1

testObject_NewClient_totp_1 :: Client.NewClient
testObject_NewClient_totp_1 =
  (Client.newClient Client.PermanentClientType (lastPrekey "test-prekey"))
    { Client.newClientPassword = Just password1,
      Client.newClientTotpCode = secondFactor1
    }

testObject_RmClient_totp_1 :: Client.RmClient
testObject_RmClient_totp_1 = Client.RmClient (Just password1) secondFactor1

testObject_CompletePasswordReset_totp_1 :: Password.CompletePasswordReset
testObject_CompletePasswordReset_totp_1 =
  Password.CompletePasswordReset
    (Password.PasswordResetEmailIdentity (User.unsafeEmailAddress "alice" "example.com"))
    (Password.PasswordResetCode (either error id (validate "reset-code")))
    (plainTextPassword8Unsafe "new-password")
    secondFactor1

testObject_AcceptTeamInvitation_totp_1 :: AcceptTeamInvitation
testObject_AcceptTeamInvitation_totp_1 =
  AcceptTeamInvitation testObject_InvitationCode_user_1 password1 secondFactor1

testObject_TeamDeleteData_totp_1 :: Team.TeamDeleteData
testObject_TeamDeleteData_totp_1 = Team.TeamDeleteData (Just password1) Nothing secondFactor1

testObject_TeamMemberDeleteData_totp_1 :: Member.TeamMemberDeleteData
testObject_TeamMemberDeleteData_totp_1 =
  Member.TeamMemberDeleteData (Just password1) secondFactor1

testObject_RemoveLegalHoldSettingsRequest_totp_1 :: LegalHold.RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_totp_1 =
  LegalHold.RemoveLegalHoldSettingsRequest (Just password1) secondFactor1

testObject_DisableLegalHoldForUserRequest_totp_1 :: LegalHold.DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_totp_1 =
  LegalHold.DisableLegalHoldForUserRequest (Just password1) secondFactor1

testObject_ApproveLegalHoldForUserRequest_totp_1 :: LegalHold.ApproveLegalHoldForUserRequest
testObject_ApproveLegalHoldForUserRequest_totp_1 =
  LegalHold.ApproveLegalHoldForUserRequest (Just password1) secondFactor1

testObject_CreateScimToken_totp_1 :: Scim.CreateScimToken
testObject_CreateScimToken_totp_1 =
  Scim.CreateScimToken "test token" (Just password1) Nothing (Just "test") Nothing secondFactor1 -- TODO: how does this even parse?  didn't we agree that mfa tokens need to have a type prefix, with the only one being planned for now being 'totp:'?

testObject_NewTOTPCredential_2 :: NewTOTPCredential
testObject_NewTOTPCredential_2 =
  testObject_NewTOTPCredential_1 {reauth = testObject_ReauthBody_2}

password1 :: PlainTextPassword6
password1 = plainTextPassword6Unsafe "test-password"
