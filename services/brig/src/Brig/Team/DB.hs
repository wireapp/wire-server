-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Team.DB
  ( module T,
    mkInviteUrl,
    insertInvitation,
    deleteInvitation,
    deleteInvitations,
    mkInvitationCode,
    mkInvitationId,
  )
where

import Brig.App as App
import Brig.Data.Types as T
import Brig.Options
import Brig.Team.Template
import Cassandra as C
import Control.Lens (view)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as C
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Range
import Data.Text.Ascii (encodeBase64Url, toText)
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Time.Clock
import Data.UUID.V4
import Imports
import OpenSSL.Random (randBytes)
import System.Logger.Class qualified as Log
import URI.ByteString
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.Team.Invitation hiding (HeadInvitationByEmailResult (..))
import Wire.API.Team.Role
import Wire.API.User
import Wire.EmailSubsystem.Template (renderTextWithBranding)
import Wire.GalleyAPIAccess (ShowOrHideInvitationUrl (..))

mkInvitationCode :: IO InvitationCode
mkInvitationCode = InvitationCode . encodeBase64Url <$> randBytes 24

mkInvitationId :: IO InvitationId
mkInvitationId = Id <$> nextRandom

insertInvitation ::
  ( Log.MonadLogger m,
    MonadReader Env m,
    MonadClient m
  ) =>
  ShowOrHideInvitationUrl ->
  InvitationId ->
  TeamId ->
  Role ->
  UTCTime ->
  Maybe UserId ->
  EmailAddress ->
  Maybe Name ->
  -- | The timeout for the invitation code.
  Timeout ->
  m (Invitation, InvitationCode)
insertInvitation showUrl iid t role (toUTCTimeMillis -> now) minviter email inviteeName timeout = do
  code <- liftIO mkInvitationCode
  url <- mkInviteUrl showUrl t code
  let inv = Invitation t role iid now minviter email inviteeName url
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery cqlInvitation (t, role, iid, code, email, now, minviter, inviteeName, round timeout)
    addPrepQuery cqlInvitationInfo (code, t, iid, round timeout)
    addPrepQuery cqlInvitationByEmail (email, t, iid, code, round timeout)
  pure (inv, code)
  where
    cqlInvitationInfo :: PrepQuery W (InvitationCode, TeamId, InvitationId, Int32) ()
    cqlInvitationInfo = "INSERT INTO team_invitation_info (code, team, id) VALUES (?, ?, ?) USING TTL ?"
    cqlInvitation :: PrepQuery W (TeamId, Role, InvitationId, InvitationCode, EmailAddress, UTCTimeMillis, Maybe UserId, Maybe Name, Int32) ()
    cqlInvitation = "INSERT INTO team_invitation (team, role, id, code, email, created_at, created_by, name) VALUES (?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?"
    -- Note: the edge case of multiple invites to the same team by different admins from the same team results in last-invite-wins in the team_invitation_email table.
    cqlInvitationByEmail :: PrepQuery W (EmailAddress, TeamId, InvitationId, InvitationCode, Int32) ()
    cqlInvitationByEmail = "INSERT INTO team_invitation_email (email, team, invitation, code) VALUES (?, ?, ?, ?) USING TTL ?"

lookupInvitationCodeEmail :: (MonadClient m) => TeamId -> InvitationId -> m (Maybe (InvitationCode, EmailAddress))
lookupInvitationCodeEmail t r = retry x1 (query1 cqlInvitationCodeEmail (params LocalQuorum (t, r)))
  where
    cqlInvitationCodeEmail :: PrepQuery R (TeamId, InvitationId) (InvitationCode, EmailAddress)
    cqlInvitationCodeEmail = "SELECT code, email FROM team_invitation WHERE team = ? AND id = ?"

deleteInvitation :: (MonadClient m) => TeamId -> InvitationId -> m ()
deleteInvitation t i = do
  codeEmail <- lookupInvitationCodeEmail t i
  case codeEmail of
    Just (invCode, invEmail) -> retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery cqlInvitation (t, i)
      addPrepQuery cqlInvitationInfo (Identity invCode)
      addPrepQuery cqlInvitationEmail (invEmail, t)
    Nothing ->
      retry x5 $ write cqlInvitation (params LocalQuorum (t, i))
  where
    cqlInvitation :: PrepQuery W (TeamId, InvitationId) ()
    cqlInvitation = "DELETE FROM team_invitation where team = ? AND id = ?"
    cqlInvitationInfo :: PrepQuery W (Identity InvitationCode) ()
    cqlInvitationInfo = "DELETE FROM team_invitation_info WHERE code = ?"
    cqlInvitationEmail :: PrepQuery W (EmailAddress, TeamId) ()
    cqlInvitationEmail = "DELETE FROM team_invitation_email WHERE email = ? AND team = ?"

deleteInvitations :: (MonadClient m) => TeamId -> m ()
deleteInvitations t =
  liftClient $
    runConduit $
      paginateC cqlSelect (paramsP LocalQuorum (Identity t) 100) x1
        .| C.mapM_ (pooledMapConcurrentlyN_ 16 (deleteInvitation t . runIdentity))
  where
    cqlSelect :: PrepQuery R (Identity TeamId) (Identity InvitationId)
    cqlSelect = "SELECT id FROM team_invitation WHERE team = ? ORDER BY id ASC"

-- | brig used to not store the role, so for migration we allow this to be empty and fill in the
-- default here.
toInvitation ::
  ( MonadReader Env m,
    Log.MonadLogger m
  ) =>
  ShowOrHideInvitationUrl ->
  ( TeamId,
    Maybe Role,
    InvitationId,
    UTCTimeMillis,
    Maybe UserId,
    EmailAddress,
    Maybe Name,
    InvitationCode
  ) ->
  m Invitation
toInvitation showUrl (t, r, i, tm, minviter, e, inviteeName, code) = do
  url <- mkInviteUrl showUrl t code
  pure $ Invitation t (fromMaybe defaultRole r) i tm minviter e inviteeName url

mkInviteUrl ::
  ( MonadReader Env m,
    Log.MonadLogger m
  ) =>
  ShowOrHideInvitationUrl ->
  TeamId ->
  InvitationCode ->
  m (Maybe (URIRef Absolute))
mkInviteUrl HideInvitationUrl _ _ = pure Nothing
mkInviteUrl ShowInvitationUrl team (InvitationCode c) = do
  template <- invitationEmailUrl . invitationEmail . snd <$> teamTemplates Nothing
  branding <- view App.templateBranding
  let url = toStrict $ renderTextWithBranding template replace branding
  parseHttpsUrl url
  where
    replace "team" = idToText team
    replace "code" = toText c
    replace x = x

    parseHttpsUrl :: (Log.MonadLogger m) => Text -> m (Maybe (URIRef Absolute))
    parseHttpsUrl url =
      either (\e -> logError url e >> pure Nothing) (pure . Just) $
        parseURI laxURIParserOptions (encodeUtf8 url)

    logError :: (Log.MonadLogger m, Show e) => Text -> e -> m ()
    logError url e =
      Log.err $
        Log.msg
          (Log.val "Unable to create invitation url. Please check configuration.")
          . Log.field "url" url
          . Log.field "error" (show e)
