module Test.Apps where

import API.BrigInternal
import SetupHelpers
import Testlib.Prelude

testAppCreation :: (HasCallStack) => App ()
testAppCreation = do
  let email = Just "alice@example.com"
  alice <- randomUser OwnDomain def {email = email}

  createUser OwnDomain def {email} >>= getJSON 300

  pure ()
