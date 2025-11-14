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

import Data.ByteString.Conversion
import Data.Id
import Data.Qualified
import Data.RetryAfter
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
import Wire.AppStore (AppStore, StoredApp (..))
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
  RefreshAppCookie lusr tid appId -> runError $ refreshAppCookieImpl lusr tid appId

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
  let app =
        StoredApp
          { id = u.id,
            teamId = tid,
            meta = new.meta
          }

  Log.info $
    Log.field "app" (toByteString app.id)
      . Log.field "creator" (toByteString creator.id)
      . Log.msg (Log.val "Creating app")

  -- create app and user entries
  Store.createApp app
  Store.createUser u Nothing

  -- generate a team event
  generateTeamEvents creator.id tid [EdAppCreate u.id]

  c :: Cookie (Token U) <- newCookie u.id Nothing PersistentCookie (Just "app")
  pure
    CreatedApp
      { user = newStoredUserToUser (tUntagged (qualifyAs lusr u)),
        cookie = mkSomeToken c.cookieValue
      }

refreshAppCookieImpl ::
  ( Member AuthenticationSubsystem r,
    Member AppStore r,
    Member (Error RetryAfter) r,
    Member (Error AppSubsystemError) r,
    Member GalleyAPIAccess r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r SomeUserToken
refreshAppCookieImpl (tUnqualified -> uid) tid appId = do
  mem <- getTeamMember uid tid >>= note AppSubsystemErrorNoPerm
  note AppSubsystemErrorNoPerm $ guard (T.hasPermission mem T.ManageApps)
  app <- Store.getApp appId >>= note AppSubsystemErrorNoApp
  note AppSubsystemErrorNoApp $ guard (app.teamId == tid)

  c :: Cookie (Token U) <-
    newCookieLimited appId Nothing PersistentCookie (Just "app")
      >>= either throw pure
  pure $ mkSomeToken c.cookieValue

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
        supportedProtocols = defAppSupportedProtocols,
        searchable = True
      }

defAppSupportedProtocols :: Set BaseProtocolTag
defAppSupportedProtocols = Set.singleton BaseProtocolMLSTag
