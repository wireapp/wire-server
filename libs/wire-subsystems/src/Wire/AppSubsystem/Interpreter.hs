module Wire.AppSubsystem.Interpreter where

import Data.ByteString.Conversion
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.UUID.V4
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.User
import Wire.AppStore (AppStore)
import Wire.AppStore qualified as Store
import Wire.AppSubsystem
import Wire.StoredUser
import Wire.UserStore

runAppSubsystem ::
  ( Member UserStore r,
    Member TinyLog r,
    Member (Embed IO) r,
    Member (Error AppSubsystemError) r,
    Member AppStore r
  ) =>
  Sem (AppSubsystem ': r) a ->
  Sem r a
runAppSubsystem = interpret \case
  CreateApp lusr new -> createAppImpl lusr new

createAppImpl ::
  ( Member UserStore r,
    Member TinyLog r,
    Member (Embed IO) r,
    Member (Error AppSubsystemError) r,
    Member AppStore r
  ) =>
  Local User ->
  NewApp ->
  Sem r ()
createAppImpl (tUnqualified -> creator) new = do
  tid <- note AppSubsystemErrorNoTeam creator.userTeam
  u <- appNewStoredUser creator new

  Log.debug $
    Log.field "user" (toByteString u.id) . Log.field "action" (Log.val "User.createUser")
  Log.info $
    Log.field "user" (toByteString u.id) . Log.msg (Log.val "Creating user")

  Store.createApp u.id tid new.meta
  createUser u Nothing

-- TODO: generate a team event

appNewStoredUser ::
  (Member (Embed IO) r) =>
  User ->
  NewApp ->
  Sem r NewStoredUser
appNewStoredUser creator new = do
  uid <- liftIO nextRandom
  let loc = creator.userLocale
  pure
    NewStoredUser
      { id = Id uid,
        email = Nothing,
        ssoId = Nothing,
        name = new.name,
        textStatus = Nothing,
        pict = new.pict,
        assets = new.assets,
        accentId = new.accentId,
        password = Nothing,
        activated = True,
        status = Active,
        language = loc.lLanguage,
        country = loc.lCountry,
        providerId = Nothing,
        serviceId = Nothing,
        handle = Nothing,
        expires = Nothing,
        teamId = creator.userTeam,
        managedBy = defaultManagedBy,
        supportedProtocols = defAppSupportedProtocols
      }

defAppSupportedProtocols :: Set BaseProtocolTag
defAppSupportedProtocols = Set.singleton BaseProtocolMLSTag
