module Wire.MockInterpreters.UserSubsystem where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.UserKeyStore
import Wire.UserSubsystem

userSubsystemTestInterpreter :: [UserAccount] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter initialUsers =
  interpret \case
    GetLocalUserAccountByUserKey localUserKey -> case (tUnqualified localUserKey) of
      EmailKey _ email -> pure $ find (\u -> userEmail u.accountUser == Just email) initialUsers
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"
