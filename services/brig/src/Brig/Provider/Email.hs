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

module Brig.Provider.Email
  ( sendActivationMail,
    sendApprovalConfirmMail,
    sendPasswordResetMail,
  )
where

import Brig.App
import Data.Code qualified as Code
import Imports
import Polysemy
import Wire.API.BackgroundJobs.Email
import Wire.API.User
import Wire.EmailSending.Queueing

sendActivationMail :: (Member EmailQueueing r) => Name -> EmailAddress -> Code.Key -> Code.Value -> Bool -> (AppT r) ()
sendActivationMail name email key code update =
  liftSem $
    queueEmail $
      ProviderActivationEmail (MkProviderActivationEmail email name key code update)

sendApprovalConfirmMail :: (Member EmailQueueing r) => Name -> EmailAddress -> (AppT r) ()
sendApprovalConfirmMail name email =
  liftSem $
    queueEmail $
      ProviderApprovalConfirmEmail (MkProviderApprovalConfirmEmail email name)

sendPasswordResetMail :: (Member EmailQueueing r) => EmailAddress -> Code.Key -> Code.Value -> (AppT r) ()
sendPasswordResetMail to key code =
  liftSem $
    queueEmail $
      ProviderPasswordResetEmail (MkProviderPasswordResetEmail to key code)
