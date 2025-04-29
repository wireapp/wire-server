module Wire.UserGroupSubsystem.Interpreter where

import Imports
import Polysemy
import Wire.UserGroupSubsystem

interpretUserGroupSubsystem :: InterpreterFor UserGroupSubsystem r
interpretUserGroupSubsystem = interpret $ \case
  CreateGroup creator newGroup -> undefined creator newGroup
  GetGroup getter gid -> undefined getter gid
