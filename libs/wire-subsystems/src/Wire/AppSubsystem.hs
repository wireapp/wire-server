module Wire.AppSubsystem where

import Data.Id
import Data.Qualified

data AppSubsystem m a where
  CreateApp :: Local UserId -> TeamId -> AppSubsystem m ()
