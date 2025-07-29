module Wire.AppSubsystem.Interpreter where

import Data.Default
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.AppSubsystem
import Wire.UserSubsystem

runAppSubsystem :: (Member UserSubsystem r) => Sem (AppSubsystem ': r) a -> Sem r a
runAppSubsystem = interpret \case
  CreateApp quid tid -> createAppImpl quid tid

createAppImpl ::
  (Member UserSubsystem r) =>
  Local UserId ->
  TeamId ->
  Sem r ()
createAppImpl quid tid = do
  -- create an account with no handle and no password
  (account, _) <- newAccount new Nothing tid Nothing

  let uid = userId account
  liftSem $ do
    Log.debug $ field "user" (toByteString uid) . field "action" (val "User.createUser")
    Log.info $ field "user" (toByteString uid) . msg (val "Creating user")

    wrapClient $ Data.insertAccount account Nothing pw False
    liftSem $ GalleyAPIAccess.createSelfConv uid
    liftSem $ Events.generateUserEvent uid Nothing (UserCreated account)

    pure account
