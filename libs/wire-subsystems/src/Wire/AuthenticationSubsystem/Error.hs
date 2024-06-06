-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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
module Wire.AuthenticationSubsystem.Error
  ( AuthenticationSubsystemError (..),
    authenticationSubsystemErrorToWai,
  )
where

import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E

data AuthenticationSubsystemError
  = AuthenticationSubsystemInvalidPasswordResetKey
  | AuthenticationSubsystemPasswordResetInProgress
  | AuthenticationSubsystemResetPasswordMustDiffer
  | AuthenticationSubSystemInvalidPasswordResetCode
  deriving (Show)

instance Exception AuthenticationSubsystemError

authenticationSubsystemErrorToWai :: AuthenticationSubsystemError -> Wai.Error
authenticationSubsystemErrorToWai =
  dynErrorToWai . \case
    AuthenticationSubsystemInvalidPasswordResetKey -> dynError @(MapError E.InvalidPasswordResetKey)
    AuthenticationSubsystemPasswordResetInProgress -> dynError @(MapError E.PasswordResetInProgress)
    AuthenticationSubSystemInvalidPasswordResetCode -> dynError @(MapError E.InvalidPasswordResetCode)
    AuthenticationSubsystemResetPasswordMustDiffer -> dynError @(MapError E.ResetPasswordMustDiffer)
