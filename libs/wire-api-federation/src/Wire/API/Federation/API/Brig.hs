module Wire.API.Federation.API.Brig where

import Data.Handle (Handle)
import Data.Id (UserId)
import Data.Qualified (Qualified)
import Imports
import Servant.API
import Servant.API.Generic

-- Maybe this module should be called Brig
newtype Api routes = Api
  { getUserByHandle ::
      routes
        :- "federation"
        :> "users"
        :> "by-handle"
        :> QueryParam' '[Required, Strict] "handle" Handle
        -- FUTUREWORK: Make this return UserProfile, at that point there would
        -- be interesting questions like whether to expose email or not and how
        -- we code that part. I want to avoid solving this until federator works
        :> Get '[JSON] (Qualified UserId)
  }
  deriving (Generic)
