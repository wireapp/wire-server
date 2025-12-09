{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.AppSubsystem where

import Data.Id
import Data.Qualified
import Data.RetryAfter
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Wire.API.App
import Wire.API.User
import Wire.API.User.Auth
import Wire.Error

data AppSubsystemConfig = AppSubsystemConfig
  { defaultLocale :: Locale
  }

data AppSubsystemError = AppSubsystemErrorNoPerm | AppSubsystemErrorNoUser | AppSubsystemErrorNoApp

appSubsystemErrorToHttpError :: AppSubsystemError -> HttpError
appSubsystemErrorToHttpError =
  StdError . \case
    AppSubsystemErrorNoPerm -> Wai.mkError status403 "app-no-permission" "User does not have permission to create or manage apps"
    AppSubsystemErrorNoUser -> Wai.mkError status403 "create-app-no-user" "App owner not found"
    AppSubsystemErrorNoApp -> Wai.mkError status404 "app-not-found" "App not found"

data AppSubsystem m a where
  CreateApp :: Local UserId -> TeamId -> NewApp -> AppSubsystem m CreatedApp
  RefreshAppCookie ::
    Local UserId ->
    TeamId ->
    UserId ->
    AppSubsystem m (Either RetryAfter SomeUserToken)

makeSem ''AppSubsystem
