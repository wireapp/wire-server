{-# LANGUAGE TemplateHaskell #-}

module Wire.AppSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Wire.API.App
import Wire.API.User
import Wire.Error

data AppSubsystemConfig = AppSubsystemConfig
  { defaultLocale :: Locale
  }

data AppSubsystemError = AppSubsystemErrorInvalidTeam | AppSubsystemErrorNoUser

appSubsystemErrorToHttpError :: AppSubsystemError -> HttpError
appSubsystemErrorToHttpError =
  StdError . \case
    AppSubsystemErrorInvalidTeam -> Wai.mkError status403 "create-app-invalid-team" "User does not have permissions to create apps"
    AppSubsystemErrorNoUser -> Wai.mkError status403 "create-app-no-user" "App owner not found"

data AppSubsystem m a where
  CreateApp :: Local UserId -> TeamId -> NewApp -> AppSubsystem m CreatedApp

makeSem ''AppSubsystem
