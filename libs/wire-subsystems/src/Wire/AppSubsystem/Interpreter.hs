-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.AppSubsystem.Interpreter where

import Control.Lens
import Data.ByteString.Conversion
import Data.Default
import Data.Id
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Misc
import Data.Qualified
import Data.RetryAfter
import Data.Set qualified as Set
import Data.ZAuth.Token (Token (..), Type (U))
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Event.Team
import Wire.API.Routes.Internal.Galley.TeamsIntra (TeamName (..))
import Wire.API.Team.Member qualified as T
import Wire.API.Team.Role qualified as R
import Wire.API.User
import Wire.API.User.Auth
import Wire.API.UserEvent hiding (UserLegalHoldDisabled)
import Wire.AppStore (AppStore, StoredApp (..))
import Wire.AppStore qualified as Store
import Wire.AppSubsystem
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Cookie (revokeAllCookies)
import Wire.AuthenticationSubsystem.ZAuth
import Wire.EmailSubsystem (AppEvent (..), EmailSubsystem)
import Wire.EmailSubsystem qualified as Email
import Wire.Events
import Wire.GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Sem.Now
import Wire.Sem.Random
import Wire.StoredUser
import Wire.TeamSubsystem
import Wire.TeamSubsystem.Util
import Wire.UserStore (UserStore)
import Wire.UserStore qualified as Store
import Wire.UserSubsystem (UserSubsystem, internalUpdateSearchIndex)

runAppSubsystem ::
  ( Member AppStore r,
    Member EmailSubsystem r,
    Member (Error AppSubsystemError) r,
    Member Events r,
    Member GalleyAPIAccess r,
    Member (Input AppSubsystemConfig) r,
    Member NotificationSubsystem r,
    Member Now r,
    Member Random r,
    Member TeamSubsystem r,
    Member TinyLog r,
    Member UserStore r
  ) =>
  InterpreterFor UserSubsystem (AuthenticationSubsystem ': r) ->
  InterpreterFor AuthenticationSubsystem r ->
  Sem (AppSubsystem ': r) a ->
  Sem r a
runAppSubsystem runUser runAuth =
  interpret $
    runAuth . runUser . \case
      CreateApp lusr tid new -> createAppImpl lusr tid new
      GetApp lusr tid uid -> getAppImpl lusr tid uid
      GetApps lusr tid -> getAppsImpl lusr tid
      UpdateApp lusr tid uid put -> updateAppImpl lusr tid uid put
      RefreshAppCookie lusr tid appId password -> runError $ refreshAppCookieImpl lusr tid appId password
      InternalDeleteApp tid appId -> internalDeleteAppImpl tid appId
      DeleteAppSendEmail tid actorId mbAppUser -> deleteAppSendEmailImpl tid actorId mbAppUser

createAppImpl ::
  ( Member UserStore r,
    Member AppStore r,
    Member TinyLog r,
    Member (Error AppSubsystemError) r,
    Member (Input AppSubsystemConfig) r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member TeamSubsystem r,
    Member NotificationSubsystem r,
    Member EmailSubsystem r,
    Member AuthenticationSubsystem r,
    Member UserSubsystem r,
    Member Random r
  ) =>
  Local UserId ->
  TeamId ->
  NewApp ->
  Sem r CreatedApp
createAppImpl lusr tid newApp = do
  verifyUserPasswordError lusr newApp.password
  (creator, mem) <- ensureTeamMember lusr tid
  note AppSubsystemErrorNoPerm $ guard (T.hasPermission mem T.CreateApp)

  u <- appNewStoredUser creator newApp
  let app =
        StoredApp
          { id = u.id,
            teamId = tid,
            meta = mempty, -- unused, can be removed from postgres schema at some point.
            category = newApp.category,
            description = newApp.description,
            creator = tUnqualified lusr
          }

  Log.info $
    Log.field "app" (toByteString app.id)
      . Log.field "creator" (toByteString creator.id)
      . Log.msg (Log.val "Creating app")

  -- create app and user entries
  Store.createApp app
  Store.createUser u Nothing
  now <- toUTCTimeMillis <$> get
  void $ addTeamMember u.id tid (Just (tUnqualified lusr, now)) R.RoleMember
  internalUpdateSearchIndex u.id

  -- generate a team event
  generateTeamEvents creator.id tid [EdMemberJoin u.id]
  createAppSendEmail creator tid newApp

  c :: Cookie (Token U) <- newCookie u.id Nothing PersistentCookie Nothing RevokeSameLabel
  pure
    CreatedApp
      { user =
          let usr :: User = newStoredUserToUser (tUntagged (qualifyAs lusr u))
              mbApp :: Maybe AppInfo = Just $ storedAppToAppInfo app
              lh = UserLegalHoldDisabled -- FUTUREWORK: this needs to be changed as soon as apps can be put under LH.
           in mkUserProfile EmailVisibleIfOnTeam usr mbApp lh,
        cookie = mkSomeToken c.cookieValue
      }

createAppSendEmail ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  StoredUser ->
  TeamId ->
  NewApp ->
  Sem r ()
createAppSendEmail creator tid newApp =
  notifyAdmins tid $ \teamName now ->
    NewAppCreated
      { actor = fromName creator.name,
        appName = newApp.name,
        date = now,
        permissions = "",
        teamName = teamName,
        teamId = tid
      }

-- | Check that @lusr@ is member of team with @tid@.
ensureTeamMember ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member (Error AppSubsystemError) r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r (StoredUser, T.TeamMember)
ensureTeamMember lusr tid = do
  storedUser <- Store.getUser (tUnqualified lusr) >>= note AppSubsystemErrorNoCreator
  teamMember <- internalGetTeamMember storedUser.id tid >>= note AppSubsystemErrorNoPerm
  pure (storedUser, teamMember)

getAppImpl ::
  ( Member AppStore r,
    Member TeamSubsystem r,
    Member (Error AppSubsystemError) r,
    Member UserStore r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r AppInfo
getAppImpl lusr tid uid = do
  void $ ensureTeamMember lusr tid
  storedApp <- Store.getApp uid tid >>= note AppSubsystemErrorNoAppData
  pure $ storedAppToAppInfo storedApp

storedAppToAppInfo :: StoredApp -> AppInfo
storedAppToAppInfo app =
  AppInfo
    { category = app.category,
      description = app.description
    }

getAppsImpl ::
  ( Member AppStore r,
    Member TeamSubsystem r,
    Member (Error AppSubsystemError) r,
    Member UserStore r
  ) =>
  Local UserId ->
  TeamId ->
  Sem r [(UserId, AppInfo)]
getAppsImpl lusr tid = do
  void $ ensureTeamMember lusr tid
  Store.getApps tid <&> map \storedApp -> (storedApp.id, storedAppToAppInfo storedApp)

updateAppImpl ::
  ( Member AppStore r,
    Member (Error AppSubsystemError) r,
    Member Events r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member TeamSubsystem r,
    Member UserStore r,
    Member UserSubsystem r,
    Member EmailSubsystem r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  PutApp ->
  Sem r ()
updateAppImpl lusr tid appid upd = do
  (updater, umem) <- ensureTeamMember lusr tid
  note AppSubsystemErrorNoPerm $ guard (T.hasPermission umem T.CreateApp)
  oldApp <- Store.getUser appid >>= note AppSubsystemErrorNoAppUser
  Store.updateApp tid appid (Store.MkStoredAppUpdate {category = upd.category, description = upd.description}) >>= \case
    Right () -> pure ()
    Left Store.NotFound -> throw AppSubsystemErrorNoAppData
  Store.updateUser appid (def {Store.name = upd.name, Store.assets = upd.assets, Store.accentId = upd.accentId})
  internalUpdateSearchIndex appid
  generateUserEvent appid Nothing $
    UserUpdated $
      (emptyUserUpdatedData appid)
        { eupName = upd.name,
          eupAccentId = upd.accentId,
          eupAssets = upd.assets
        }
  updateAppMaybeSendEmail updater tid oldApp.name (fromMaybe oldApp.name upd.name)

updateAppMaybeSendEmail ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  StoredUser ->
  TeamId ->
  Name ->
  Name ->
  Sem r ()
updateAppMaybeSendEmail updater tid oldName newName = when (newName /= oldName) do
  notifyAdmins tid $ \teamName now ->
    AppMetadataChanged
      { actor = fromName updater.name,
        date = now,
        newAppName = newName,
        previousAppName = oldName,
        teamName = teamName,
        teamId = tid
      }

refreshAppCookieImpl ::
  ( Member AuthenticationSubsystem r,
    Member AppStore r,
    Member (Error RetryAfter) r,
    Member (Error AppSubsystemError) r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member TeamSubsystem r,
    Member UserStore r,
    Member EmailSubsystem r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Maybe PlainTextPassword6 ->
  Sem r SomeUserToken
refreshAppCookieImpl (tUnqualified -> uid) tid appId mbPassword = do
  reauthenticateEither uid mbPassword
    >>= either (const $ throw AppSubsystemErrorMissingAuth) (const $ pure ())

  mem <- internalGetTeamMember uid tid >>= note AppSubsystemErrorNoPerm
  note AppSubsystemErrorNoPerm $ guard (T.hasPermission mem T.ManageApps)
  void $ Store.getApp appId tid >>= note AppSubsystemErrorNoAppData

  revokeAllCookies appId
  c :: Cookie (Token U) <-
    newCookieLimited appId Nothing PersistentCookie Nothing RevokeSameLabel
      >>= either throw pure
  refreshAppCookieSendEmail uid tid appId
  pure $ mkSomeToken c.cookieValue

refreshAppCookieSendEmail ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member (Error AppSubsystemError) r,
    Member EmailSubsystem r
  ) =>
  UserId ->
  TeamId ->
  UserId ->
  Sem r ()
refreshAppCookieSendEmail actorId tid appId = do
  appUser <- Store.getUser appId >>= note AppSubsystemErrorNoAppUser
  actor <- appEventActor actorId
  notifyAdmins tid $ \teamName now ->
    AppTokenChanged
      { actor = actor,
        appName = appUser.name,
        date = now,
        teamName = teamName,
        teamId = tid
      }

appNewStoredUser ::
  (Member (Input AppSubsystemConfig) r, Member Random r) =>
  StoredUser ->
  NewApp ->
  Sem r NewStoredUser
appNewStoredUser creator new = do
  uid <- newId
  defLoc <- inputs defaultLocale
  let loc = toLocale defLoc (creator.language, creator.country)
  pure
    NewStoredUser
      { id = uid,
        userType = UserTypeApp,
        email = Nothing,
        ssoId = Nothing,
        name = new.name,
        textStatus = Nothing,
        pict = Pict [],
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
        supportedProtocols = defAppSupportedProtocols,
        searchable = True
      }

defAppSupportedProtocols :: Set BaseProtocolTag
defAppSupportedProtocols = Set.singleton BaseProtocolMLSTag

-- | Send the notification email for an 'AppEvent' to every team admin/owner.
-- Admins without an email address are silently skipped.
-- The callback receives the team name and current timestamp; both are fetched here.
notifyAdmins ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  TeamId ->
  (Text -> UTCTimeMillis -> AppEvent) ->
  Sem r ()
notifyAdmins tid makeEvent = do
  teamName <- tnName <$> getTeamName tid
  now <- toUTCTimeMillis <$> get
  let event = makeEvent teamName now
  admins <- internalGetTeamAdmins tid
  let adminUids = admins ^.. T.teamMembers . traverse . T.userId
  adminUsers <- Store.getUsers adminUids
  forM_ adminUsers $ \u ->
    for_ u.email $ \email ->
      Email.sendAppEventEmail email u.name tid event u.locale

-- | Best-effort actor display name for an 'AppEvent'.
appEventActor :: (Member UserStore r) => UserId -> Sem r Text
appEventActor actorId = maybe "-/-" (fromName . (.name)) <$> Store.getUser actorId

-- | TODO: no operation toggles app availability yet. This helper wires the
-- 'AppAvailabilityChanged' event end-to-end; call it from the availability
-- toggle once that operation exists.
_changeAppAvailabilitySendEmail ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  TeamId ->
  UserId ->
  UserId ->
  Text ->
  Text ->
  Sem r ()
_changeAppAvailabilitySendEmail tid appId actorId previousAvailability newAvailability = do
  appName <- maybe (Name "-/-") (.name) <$> Store.getUser appId
  actor <- appEventActor actorId
  notifyAdmins tid $ \teamName now ->
    AppAvailabilityChanged
      { actor = actor,
        appName = appName,
        date = now,
        newAvailability = newAvailability,
        previousAvailability = previousAvailability,
        teamName = teamName,
        teamId = tid
      }

internalDeleteAppImpl :: (Member AppStore r) => TeamId -> UserId -> Sem r ()
internalDeleteAppImpl teamId appId = do
  Store.deleteApp appId teamId

deleteAppSendEmailImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  TeamId ->
  UserId ->
  Maybe User ->
  Sem r ()
deleteAppSendEmailImpl tid actorId mbAppUser = do
  let appName = maybe (Name "-/-") (.userDisplayName) mbAppUser
  actor <- appEventActor actorId
  notifyAdmins tid $ \teamName now ->
    AppDeleted
      { actor = actor,
        appName = appName,
        date = now,
        teamName = teamName,
        teamId = tid
      }
