module Wire.AppSubsystem where

import Data.Aeson
import Data.Qualified
import Imports
import Wire.API.User

data NewApp = NewApp
  { name :: Name,
    pict :: Pict,
    assets :: [Asset],
    accentId :: ColourId,
    meta :: Object
  }

defNewApp :: Name -> NewApp
defNewApp name =
  NewApp
    { name,
      pict = noPict,
      assets = [],
      accentId = defaultAccentId,
      meta = mempty
    }

data AppSubsystemError = AppSubsystemErrorNoTeam

data AppSubsystem m a where
  CreateApp :: Local User -> NewApp -> AppSubsystem m ()
