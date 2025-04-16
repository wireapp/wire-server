{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

module Wire.UserGroupSubsystem.InterpreterSpec (spec) where

import Data.Id
import Data.Time
import Data.UUID as UUID
import Imports
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User hiding (DeleteUser)
import Wire.API.UserGroup
import Wire.MockInterpreters.UserGroupSubsystem qualified as Mock
import Wire.UserGroupSubsystem

spec :: Spec
spec = describe "UserGroupSubsystem.Interpreter" do
  focus . prop "getGroup gets you what createGroup creates" $ \gname usrs ->
    let now = unsafePerformIO getCurrentTime
     in Mock.runInMemoryUserGroupSubsystem now do
          ug0 <- getGroup (Id UUID.nil)
          let nug = NewUserGroup gname usrs
          ug <- createGroup nug
          ug' <- getGroup ug.id_
          pure $
            counterexample (show (ug0, nug, ug, ug')) $
              (ug0 === Nothing)
                .&&. (ug.name === nug.name)
                .&&. (ug.members === nug.members)
                .&&. (ug.managedBy === ManagedByWire)
                .&&. (ug.createdAt === now)
                .&&. (ug' === Just ug)

  prop "getGroups gets all groups" $ \b -> b === True

  prop "getGroups paginates" $ \b -> b === True

  prop "getGroups may loose entries added after inpropializing the paging state" $ \b -> b === True

  prop "updateGroup updates" $ \b -> b === True

  prop "deleteGroup deletes" $ \b -> b === True

  prop "addUser adds a user" $ \b -> b === True

  prop "removeUser removes a user" $ \b -> b === True
