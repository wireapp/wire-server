module Wire.MockInterpreters.UserGroupSubsystem (userGroupSubsystemTestInterpreter) where

import Data.Id
import Data.Time
import GHC.Stack
import Imports
import Polysemy
import Wire.API.UserGroup
import Wire.UserGroupSubsystem

userGroupSubsystemTestInterpreter :: UTCTime -> InterpreterFor UserGroupSubsystem r
userGroupSubsystemTestInterpreter _now =
  interpret \case
    CreateGroup ng -> createGroupImpl ng
    GetGroup gid -> getGroupImpl gid
    GetGroups pageState -> getGroupsImpl pageState
    UpdateGroup gid gup -> updateGroupImpl gid gup
    DeleteGroup gid -> deleteGroupImpl gid
    AddUser gid uid -> addUserImpl gid uid
    RemoveUser gid uid -> removeUserImpl gid uid

createGroupImpl :: (HasCallStack) => NewUserGroup -> Sem r UserGroup
createGroupImpl = undefined

getGroupImpl :: (HasCallStack) => UserGroupId -> Sem r (Maybe UserGroup)
getGroupImpl = undefined

getGroupsImpl :: (HasCallStack) => Text -> Sem r UserGroupPage
getGroupsImpl = undefined

updateGroupImpl :: (HasCallStack) => UserGroupId -> UserGroupUpdate -> Sem r UserGroup
updateGroupImpl = undefined

deleteGroupImpl :: (HasCallStack) => UserGroupId -> Sem r ()
deleteGroupImpl = undefined

addUserImpl :: (HasCallStack) => UserGroupId -> UserId -> Sem r ()
addUserImpl = undefined

removeUserImpl :: (HasCallStack) => UserGroupId -> UserId -> Sem r ()
removeUserImpl = undefined
