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

module Brig.Email
  ( -- * Validation
    validateEmail,

    -- * Re-exports
    Email (..),
    EmailKey,
    mkEmailKey,
    emailKeyUniq,
    emailKeyOrig,

    -- * MIME Re-exports
    Mail (..),
    emptyMail,
    plainPart,
    htmlPart,
    Address (..),
    mkMimeAddress,
    sendMail,
  )
where

import qualified Brig.AWS as AWS
import Brig.App (Env, awsEnv, smtpEnv)
import qualified Brig.SMTP as SMTP
import Brig.Types.Common
import Control.Lens (view)
import qualified Data.Text as Text
import Imports
import Network.Mail.Mime
import Wire.API.User

-------------------------------------------------------------------------------
sendMail :: (MonadIO m, MonadReader Env m) => Mail -> m ()
sendMail m =
  view smtpEnv >>= \case
    Just smtp -> SMTP.sendMail smtp m
    Nothing -> view awsEnv >>= \e -> AWS.execute e $ AWS.sendMail m

-------------------------------------------------------------------------------
-- MIME Conversions

-- | Construct a MIME 'Address' from the given display 'Name' and 'Email'
-- address that does not exceed 320 bytes in length when rendered for use
-- in SMTP, which is a safe limit for most mail servers (including those of
-- Amazon SES). The display name is only included if it fits within that
-- limit, otherwise it is dropped.
mkMimeAddress :: Name -> Email -> Address
mkMimeAddress name email =
  let addr = Address (Just (fromName name)) (fromEmail email)
   in if Text.compareLength (renderAddress addr) 320 == GT
        then Address Nothing (fromEmail email)
        else addr
