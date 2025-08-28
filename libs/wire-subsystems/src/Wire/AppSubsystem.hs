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

data AppSubsystemError = AppSubsystemErrorNoTeam | AppSubsystemErrorNoUser

appSubsystemErrorToHttpError :: AppSubsystemError -> HttpError
appSubsystemErrorToHttpError =
  StdError . \case
    AppSubsystemErrorNoTeam -> Wai.mkError status403 "create-app-no-team" "Apps cannot be created by personal users"
    AppSubsystemErrorNoUser -> Wai.mkError status403 "create-app-no-user" "App owner not found"

data AppSubsystem m a where
  CreateApp :: Local UserId -> NewApp -> AppSubsystem m CreatedApp

makeSem ''AppSubsystem
