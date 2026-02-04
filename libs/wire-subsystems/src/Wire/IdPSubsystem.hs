module Wire.IdPSubsystem where

import Imports
import Wire.API.Routes.Public (ZHostValue)

data IdPSubsystem m a where
  GetSsoCodeByEmail :: Maybe ZHostValue -> IdPSubsystem m ()
