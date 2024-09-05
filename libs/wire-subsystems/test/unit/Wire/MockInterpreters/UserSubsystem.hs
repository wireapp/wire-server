module Wire.MockInterpreters.UserSubsystem where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.UserKeyStore
import Wire.UserSubsystem

userSubsystemTestInterpreter :: [ExtendedUserAccount] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter initialUsers =
  interpret \case
    GetExtendedAccountsBy (tSplit -> (_dom, getBy)) ->
      pure $
        filter
          ( \u ->
              mailKeyFrom u
                `elem` getBy.getByEmail
          )
          initialUsers
    _ -> error $ "userSubsystemTestInterpreter: implement on demand"

mailKeyFrom :: ExtendedUserAccount -> EmailKey
mailKeyFrom acc =
  case acc.account.accountUser.userIdentity of
    Just (EmailIdentity mail) -> mkEmailKey mail
    Just (SSOIdentity _ (Just mail)) -> mkEmailKey mail
    _ -> error "Why are we testing users without emails for this?"
