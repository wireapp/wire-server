module Wire.MockInterpreters.UserSubsystem where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.UserSubsystem

-- HINT: This is used to test AuthenticationSubsystem, not to test itself!
userSubsystemTestInterpreter :: [User] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter initialUsers =
  interpret \case
    GetExtendedAccountsByEmailNoFilter (tUnqualified -> emails) ->
      pure $
        filter
          (\u -> userEmail u `elem` (Just <$> emails))
          initialUsers
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"
