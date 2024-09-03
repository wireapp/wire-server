module Wire.MockInterpreters.UserSubsystem where

import Debug.Trace (traceM)
import Imports
import Polysemy
import Wire.API.User
import Wire.UserSubsystem

userSubsystemTestInterpreter :: [UserAccount] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter _initialUsers =
  interpret \case
    GetAccountsBy getBy -> do
      traceM $ "\n getBy: " <> show getBy
      pure []
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"

-- case (tUnqualified localUserKey) of
-- EmailKey _ email -> pure $ find (\u -> userEmail u.accountUser == Just email) initialUsers
