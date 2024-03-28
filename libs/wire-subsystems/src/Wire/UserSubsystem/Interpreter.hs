{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.Interpreter where

import Data.Id
import Data.LegalHold
import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.UserStore

getUserProfileImpl :: Member UserStore r => UserId -> Qualified UserId -> Sem r (Maybe UserProfile)
getUserProfileImpl _profileViewer quid =
  flip publicProfile UserLegalHoldDisabled <$$> getUser (qUnqualified quid)
