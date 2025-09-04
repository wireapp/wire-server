module Wire.AppSubsystem.Interpreter where

import Data.ByteString.Conversion
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.UUID.V4
import Data.ZAuth.Token
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.App
import Wire.API.Event.Team
import Wire.API.Team.Member qualified as T
import Wire.API.User
import Wire.API.User.Auth
import Wire.AppStore (AppStore)
import Wire.AppStore qualified as Store
import Wire.AppSubsystem
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.ZAuth
import Wire.GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.StoredUser
import Wire.TeamSubsystem
import Wire.TeamSubsystem.Util
import Wire.UserStore (UserStore)
import Wire.UserStore qualified as Store

runAppSubsystem ::
  ( Member UserStore r,
    Member TinyLog r,
    Member (Embed IO) r,
    Member (Error AppSubsystemError) r,
    Member (Input AppSubsystemConfig) r,
    Member GalleyAPIAccess r,
    Member AppStore r,
    Member Now r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r,
    Member AuthenticationSubsystem r
  ) =>
  Sem (AppSubsystem ': r) a ->
  Sem r a
runAppSubsystem = interpret \case
  CreateApp lusr tid new -> createAppImpl lusr tid new

createAppImpl ::
  ( Member UserStore r,
    Member TinyLog r,
    Member (Embed IO) r,
    Member (Error AppSubsystemError) r,
    Member (Input AppSubsystemConfig) r,
    Member GalleyAPIAccess r,
    Member AppStore r,
    Member Now r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r,
    Member AuthenticationSubsystem r
  ) =>
  Local UserId ->
  TeamId ->
  NewApp ->
  Sem r CreatedApp
createAppImpl lusr tid new = do
  creator <- Store.getUser (tUnqualified lusr) >>= note AppSubsystemErrorNoUser

  mem <- getTeamMember creator.id tid >>= note AppSubsystemErrorNoPerm
  note AppSubsystemErrorNoPerm $ guard (T.hasPermission mem T.CreateApp)

  u <- appNewStoredUser creator new

  Log.debug $
    Log.field "user" (toByteString u.id) . Log.field "action" (Log.val "User.createUser")
  Log.info $
    Log.field "user" (toByteString u.id) . Log.msg (Log.val "Creating user")

  -- create app and user entries
  Store.createApp u.id tid new.meta
  Store.createUser u Nothing

  -- generate a team event
  generateTeamEvent creator.id tid (EdAppCreate u.id)

  c :: Cookie (Token U) <- newCookie u.id Nothing PersistentCookie (Just "app")
  pure
    CreatedApp
      { user = newStoredUserToUser (tUntagged (qualifyAs lusr u)),
        cookie = mkSomeToken c.cookieValue
      }

appNewStoredUser ::
  ( Member (Embed IO) r,
    Member (Input AppSubsystemConfig) r
  ) =>
  StoredUser ->
  NewApp ->
  Sem r NewStoredUser
appNewStoredUser creator new = do
  uid <- liftIO nextRandom
  defLoc <- inputs defaultLocale
  let loc = toLocale defLoc (creator.language, creator.country)
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
        teamId = creator.teamId,
        managedBy = defaultManagedBy,
        supportedProtocols = defAppSupportedProtocols
      }

defAppSupportedProtocols :: Set BaseProtocolTag
defAppSupportedProtocols = Set.singleton BaseProtocolMLSTag
