module Wire.MockInterpreters.UserSubsystem where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.UserSubsystem

-- HINT: This is used to test AuthenticationSubsystem, not to test itself!
userSubsystemTestInterpreter :: [ExtendedUserAccount] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter initialUsers =
  interpret \case
    GetLocalExtendedAccountsByEmail (tUnqualified -> emails) ->
      pure $
        filter
          (\u -> userEmail u.account.accountUser `elem` (Just <$> emails))
          initialUsers
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"
