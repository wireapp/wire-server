module Wire.API.App where

import Data.Aeson qualified as A
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.User

data NewApp = NewApp
  { name :: Name,
    pict :: Pict,
    assets :: [Asset],
    accentId :: ColourId,
    meta :: A.Object
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema NewApp

instance ToSchema NewApp where
  schema =
    object "NewApp" $
      NewApp
        <$> (.name) .= field "name" schema
        <*> (.pict) .= (fromMaybe noPict <$> optField "picture" schema)
        <*> (.assets) .= (fromMaybe [] <$> optField "assets" (array schema))
        <*> (.accentId) .= (fromMaybe defaultAccentId <$> optField "accent_id" schema)
        <*> (.meta) .= field "metadata" jsonObject

defNewApp :: Name -> NewApp
defNewApp name =
  NewApp
    { name,
      pict = noPict,
      assets = [],
      accentId = defaultAccentId,
      meta = mempty
    }

data CreatedApp = CreatedApp
  { user :: User,
    -- TODO
    cookie :: ()
  }
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema CreatedApp

instance ToSchema CreatedApp where
  schema =
    object "CreatedApp" $
      CreatedApp
        <$> (.user) .= field "user" schema
        <*> (.cookie) .= field "cookie" s
    where
      s :: ValueSchema SwaggerDoc ()
      s = null_ -- TODO
