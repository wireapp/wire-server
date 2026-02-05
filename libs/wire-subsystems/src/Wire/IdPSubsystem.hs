module Wire.IdPSubsystem where

import Imports
import Wire.API.Routes.Public (ZHostValue)
import Wire.API.User (EmailAddress)

data IdPSubsystem m a where
  GetSsoCodeByEmail :: Maybe ZHostValue -> EmailAddress -> IdPSubsystem m ()
