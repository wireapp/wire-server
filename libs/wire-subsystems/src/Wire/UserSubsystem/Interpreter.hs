module Wire.UserSubsystem.Interpreter where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.User

getUserProfileImpl :: Member UserStore r => UserId -> Qualified UserId -> Sem r (Maybe UserProfile)
getUserProfileImpl _profileViewer _quid = undefined
