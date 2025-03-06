module Wire.MockInterpreters.UserSubsystem where

import Data.Qualified
import Imports
import Polysemy
import Wire.API.User
import Wire.UserSubsystem

userSubsystemTestInterpreter :: [User] -> InterpreterFor UserSubsystem r
userSubsystemTestInterpreter initialUsers =
  interpret \case
    GetAccountsByEmailNoFilter (tUnqualified -> emails) ->
      pure $
        filter
          (\u -> userEmail u `elem` (Just <$> emails))
          initialUsers
    GetUserProfiles _ _ -> error "GetUserProfiles: implement on demand (userSubsystemInterpreter)"
    GetUserProfilesWithErrors _ _ -> error "GetUserProfilesWithErrors: implement on demand (userSubsystemInterpreter)"
    GetLocalUserProfiles _ -> error "GetLocalUserProfiles: implement on demand (userSubsystemInterpreter)"
    GetAccountsBy _ -> error "GetAccountsBy: implement on demand (userSubsystemInterpreter)"
    GetAccountNoFilter _ -> error "GetAccountNoFilter: implement on demand (userSubsystemInterpreter)"
    GetSelfProfile uid -> pure . fmap SelfProfile $ find (\u -> qUnqualified u.userQualifiedId == tUnqualified uid) initialUsers
    UpdateUserProfile {} -> error "UpdateUserProfile: implement on demand (userSubsystemInterpreter)"
    CheckHandle _ -> error "CheckHandle: implement on demand (userSubsystemInterpreter)"
    CheckHandles _ _ -> error "CheckHandles: implement on demand (userSubsystemInterpreter)"
    UpdateHandle {} -> error "UpdateHandle: implement on demand (userSubsystemInterpreter)"
    LookupLocaleWithDefault _ -> error "LookupLocaleWithDefault: implement on demand (userSubsystemInterpreter)"
    GuardRegisterActivateUserEmailDomain {} -> error "GuardRegisterActivateUserEmailDomain: implemented on demand (userSubsystemInterpreter)"
    GuardUpgradePersonalUserToTeamEmailDomain {} -> error "GuardUpgradePersonalUserToTeamEmailDomain: implemented on demand (userSubsystemInterpreter)"
    IsBlocked _ -> pure False
    BlockListDelete _ -> error "BlockListDelete: implement on demand (userSubsystemInterpreter)"
    BlockListInsert _ -> error "BlockListInsert: implement on demand (userSubsystemInterpreter)"
    UpdateTeamSearchVisibilityInbound _ -> error "UpdateTeamSearchVisibilityInbound: implement on demand (userSubsystemInterpreter)"
    AcceptTeamInvitation {} -> error "AcceptTeamInvitation: implement on demand (userSubsystemInterpreter)"
    InternalUpdateSearchIndex _ -> error "InternalUpdateSearchIndex: implement on demand (userSubsystemInterpreter)"
    InternalFindTeamInvitation {} -> error "InternalFindTeamInvitation: implement on demand (userSubsystemInterpreter)"
    GetUserExportData _ -> error "GetUserExportData: implement on demand (userSubsystemInterpreter)"
    RemoveEmailEither _ -> error "RemoveEmailEither: implement on demand (userSubsystemInterpreter)"
    SearchUsers {} -> error "SearchUsers: implement on demand (userSubsystemInterpreter)"
    BrowseTeam {} -> error "BrowseTeam: implement on demand (userSubsystemInterpreter)"
