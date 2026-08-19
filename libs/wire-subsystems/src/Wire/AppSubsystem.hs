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

import Data.Default
import Data.Id
import Data.Misc
import Data.Qualified
import Data.RetryAfter
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Wire.API.User
import Wire.API.User.Auth
import Wire.Error

data AppSubsystemConfig = AppSubsystemConfig
  { defaultLocale :: Locale
  }

instance Default AppSubsystemConfig where
  def = AppSubsystemConfig def

data AppSubsystemError
  = AppSubsystemErrorNoPerm
  | AppSubsystemErrorMissingAuth
  | AppSubsystemErrorNoCreator
  | AppSubsystemErrorNoAppData
  | AppSubsystemErrorNoAppUser
  | AppSubsystemErrorNoAppTeamMember
  deriving (Eq, Show)

instance Exception AppSubsystemError

appSubsystemErrorToHttpError :: AppSubsystemError -> HttpError
appSubsystemErrorToHttpError =
  StdError . \case
    AppSubsystemErrorNoPerm -> Wai.mkError status403 "app-no-permission" "User does not have permission to create or manage apps"
    AppSubsystemErrorMissingAuth -> Wai.mkError status403 "missing-auth" "Re-authentication via password required"
    AppSubsystemErrorNoCreator -> Wai.mkError status403 "no-app-creator" "App owner not found"
    AppSubsystemErrorNoAppData -> Wai.mkError status404 "app-not-found" "App not found (metadata record)"
    AppSubsystemErrorNoAppUser -> Wai.mkError status404 "app-not-found" "App not found (user record)"
    AppSubsystemErrorNoAppTeamMember -> Wai.mkError status404 "app-not-found" "App not found (team member record)"

data AppSubsystem m a where
  CreateApp :: Local UserId -> TeamId -> NewApp -> AppSubsystem m CreatedApp
  GetApp :: Local UserId -> TeamId -> UserId -> AppSubsystem m AppInfo
  GetApps :: Local UserId -> TeamId -> AppSubsystem m [(UserId, AppInfo)]
  UpdateApp :: Local UserId -> TeamId -> UserId -> PutApp -> AppSubsystem m ()
  RefreshAppCookie ::
    Local UserId ->
    TeamId ->
    UserId ->
    Maybe PlainTextPassword6 ->
    AppSubsystem m (Either RetryAfter SomeUserToken)
  -- | Delete app.  This is called when deleting team members.  It
  -- does not check authentication.
  --
  -- Don't forget to call InternalDeleteAppSendEmail when you call
  -- DeleteApp!
  --
  -- Rationale: sending the email notification requires some state
  -- that is lost during deletion.  By calling it explicitly in the
  -- logic of team member deletion (where everything else happens)
  -- makes it harder to get it wrong.
  InternalDeleteApp :: TeamId -> UserId -> AppSubsystem m ()
  DeleteAppSendEmail :: TeamId -> UserId -> Maybe User -> AppSubsystem m ()

makeSem ''AppSubsystem

getAppIds :: (Member AppSubsystem r) => Local UserId -> TeamId -> Sem r [UserId]
getAppIds self tid = fst <$$> getApps self tid
