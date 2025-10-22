{-# LANGUAGE TemplateHaskell #-}

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
