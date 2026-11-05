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

module Brig.API.TOTP.Mock (totpAPI) where

import Brig.API.Handler (Handler)
import Brig.App (liftSem)
import Data.Handle (fromHandle)
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Qualified (Local, tUnqualified)
import Data.Time (NominalDiffTime, addUTCTime)
import Imports
import Network.Wai.Utilities.Headers (CacheControl (NoStore))
import Polysemy (Member, Sem)
import Servant hiding (Handler)
import Test.QuickCheck qualified as QC
import Wire.API.Routes.Named (Named (Named))
import Wire.API.Routes.Public.Brig.TOTP
import Wire.API.User (SelfProfile (selfUser), userHandle)
import Wire.API.User.Auth.TOTP (ActivateTOTPCredential, NewTOTPCredential, ReauthBody, TOTPCredentialInfo, TOTPCredentialName, TOTPEnrolment (..), TOTPHashAlgorithm (..), TOTPRecoveryCodes (..), otpauthURI)
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.UserSubsystem (UserSubsystem, getSelfProfile)

totpAPI ::
  ( Member Now r,
    Member UserSubsystem r
  ) =>
  ServerT TOTPAPI (Handler r)
totpAPI =
  Named @"totp-enrol" mockEnrol
    :<|> Named @"totp-activate" mockActivate
    :<|> Named @"totp-list" mockList
    :<|> Named @"totp-rename" mockRename
    :<|> Named @"totp-delete" mockDelete

-- | The whole mock, modulo the fixups below.
arbitraryIO :: (QC.Arbitrary a, MonadIO m) => m a
arbitraryIO = liftIO (QC.generate QC.arbitrary)

--------------------------------------------------------------------------------
-- The fixups

mockIssuer :: Text
mockIssuer = "Wire"

mockAlgorithm :: TOTPHashAlgorithm
mockAlgorithm = TOTPSHA1

mockDigits :: Word8
mockDigits = 6

mockPeriod :: Word16
mockPeriod = 30

-- | Long enough to install an app and scan a QR code.
mockEnrolmentTTL :: NominalDiffTime
mockEnrolmentTTL = 15 * 60

-- | How many recovery codes activation hands out.
mockRecoveryCodeCount :: Int
mockRecoveryCodeCount = 10

-- | At most this many credentials in a listing, so clients exercise both the
-- empty and the populated rendering.
mockMaxListed :: Int
mockMaxListed = 3

--------------------------------------------------------------------------------
-- Handlers

mockEnrol ::
  ( Member Now r,
    Member UserSubsystem r
  ) =>
  Local UserId ->
  NewTOTPCredential ->
  Handler r (TOTPEnrolment, CacheControl)
mockEnrol lusr _body = do
  random <- arbitraryIO @TOTPEnrolment
  now <- lift . liftSem $ Now.get
  label <- lift . liftSem $ accountLabel lusr
  let enrolment =
        TOTPEnrolment
          { credentialId = random.credentialId,
            secret = random.secret,
            -- a real URI, built from the secret in this very
            -- response.  A client that renders this QR code and scans
            -- it must get an entry that is at least self-consistent.
            uri = otpauthURI label random.secret mockAlgorithm mockDigits mockPeriod mockIssuer,
            algorithm = mockAlgorithm,
            digits = mockDigits,
            period = mockPeriod,
            serverTime = toUTCTimeMillis now,
            expiresAt = toUTCTimeMillis (addUTCTime mockEnrolmentTTL now)
          }
  pure (enrolment, NoStore)

mockActivate ::
  Local UserId ->
  TOTPCredentialId ->
  ActivateTOTPCredential ->
  Handler r TOTPRecoveryCodes
mockActivate _lusr _cid _body =
  TOTPRecoveryCodes <$> liftIO (QC.generate (replicateM mockRecoveryCodeCount QC.arbitrary))

mockList :: Local UserId -> Handler r [TOTPCredentialInfo]
mockList _lusr = liftIO (QC.generate (QC.resize mockMaxListed QC.arbitrary))

mockRename :: Local UserId -> TOTPCredentialId -> TOTPCredentialName -> Handler r ()
mockRename _lusr _cid _body = pure ()

mockDelete :: Local UserId -> TOTPCredentialId -> ReauthBody -> Handler r ()
mockDelete _lusr _cid _body = pure ()

--------------------------------------------------------------------------------
-- The otpauth:// URI

-- | What the authenticator app shows next to the issuer.  The developer
-- testing this sees their own handle, which is worth the one effect
-- dependency; accounts without a handle fall back to the user id.
accountLabel :: (Member UserSubsystem r) => Local UserId -> Sem r Text
accountLabel lusr = do
  mProfile <- getSelfProfile lusr
  pure $ case mProfile of
    Just profile | Just h <- profile.selfUser.userHandle -> fromHandle h
    _ -> idToText (tUnqualified lusr)
