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
    GetAccountsBy (tSplit -> (_dom, getBy)) ->
      pure $
        filter
          ( \u ->
              mailKeyFrom u
                `elem` getBy.getByEmail
          )
          initialUsers
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"

mailKeyFrom :: UserAccount -> EmailKey
mailKeyFrom acc =
  case acc.accountUser.userIdentity of
    Just (EmailIdentity mail) -> mkEmailKey mail
    Just (SSOIdentity _ (Just mail)) -> mkEmailKey mail
    _ -> error "Why are we testing users without emails for this?"
