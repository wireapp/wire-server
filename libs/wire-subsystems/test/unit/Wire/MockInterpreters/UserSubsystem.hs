module Wire.MockInterpreters.UserSubsystem where

import Imports
import Polysemy
import Wire.API.User
import Wire.UserSubsystem

userSubsystemTestInterpreter :: [UserAccount] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter _initialUsers =
  interpret \case
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"
