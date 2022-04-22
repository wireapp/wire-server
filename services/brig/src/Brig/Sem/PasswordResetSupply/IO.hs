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

module Brig.Sem.PasswordResetSupply.IO (passwordResetSupplyToIO) where

import Brig.Sem.PasswordResetSupply
import Brig.Types
import Data.ByteString.Conversion
import Data.Id
import Data.Text
import Data.Text.Ascii
import Imports
import OpenSSL.BN
import OpenSSL.EVP.Digest
import OpenSSL.Random
import Polysemy
import Text.Printf

passwordResetSupplyToIO ::
  forall r a.
  (Member (Embed IO) r) =>
  Sem (PasswordResetSupply ': r) a ->
  Sem r a
passwordResetSupplyToIO =
  interpret $
    embed @IO
      . \case
        MkPasswordResetKey uid -> mkPwdResetKey uid
        GenerateEmailCode -> genEmailCode
        GeneratePhoneCode -> genPhoneCode

genEmailCode :: MonadIO m => m PasswordResetCode
genEmailCode = PasswordResetCode . encodeBase64Url <$> liftIO (randBytes 24)

genPhoneCode :: MonadIO m => m PasswordResetCode
genPhoneCode =
  PasswordResetCode . unsafeFromText . pack . printf "%06d"
    <$> liftIO (randIntegerZeroToNMinusOne 1000000)

mkPwdResetKey :: MonadIO m => UserId -> m PasswordResetKey
mkPwdResetKey u = do
  d <- liftIO $ getDigestByName "SHA256" >>= maybe (error "SHA256 not found") return
  return . PasswordResetKey . encodeBase64Url . digestBS d $ toByteString' u
