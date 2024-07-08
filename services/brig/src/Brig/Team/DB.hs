{-# LANGUAGE RecordWildCards #-}

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
    countInvitations,
    insertInvitation,
    deleteInvitation,
    deleteInvitations,
    lookupInvitation,
    lookupInvitationCode,
    lookupInvitations,
    lookupInvitationByCode,
    lookupInvitationInfo,
    lookupInvitationInfoByEmail,
    lookupInvitationByEmail,
    mkInvitationCode,
    mkInvitationId,
    InvitationByEmail (..),
    InvitationInfo (..),
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

data InvitationInfo = InvitationInfo
  { iiCode :: InvitationCode,
    iiTeam :: TeamId,
    iiInvId :: InvitationId
  }
  deriving (Eq, Show)

data InvitationByEmail
  = InvitationByEmail InvitationInfo
  | InvitationByEmailNotFound
  | InvitationByEmailMoreThanOne

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
  Email ->
  Maybe Name ->
  Maybe Phone ->
  -- | The timeout for the invitation code.
  Timeout ->
  m (Invitation, InvitationCode)
insertInvitation showUrl iid t role (toUTCTimeMillis -> now) minviter email inviteeName phone timeout = do
  code <- liftIO mkInvitationCode
  url <- mkInviteUrl showUrl t code
  let inv = Invitation t role iid now minviter email inviteeName phone url
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery cqlInvitation (t, role, iid, code, email, now, minviter, inviteeName, phone, round timeout)
    addPrepQuery cqlInvitationInfo (code, t, iid, round timeout)
    addPrepQuery cqlInvitationByEmail (email, t, iid, code, round timeout)
  pure (inv, code)
  where
    cqlInvitationInfo :: PrepQuery W (InvitationCode, TeamId, InvitationId, Int32) ()
    cqlInvitationInfo = "INSERT INTO team_invitation_info (code, team, id) VALUES (?, ?, ?) USING TTL ?"
    cqlInvitation :: PrepQuery W (TeamId, Role, InvitationId, InvitationCode, Email, UTCTimeMillis, Maybe UserId, Maybe Name, Maybe Phone, Int32) ()
    cqlInvitation = "INSERT INTO team_invitation (team, role, id, code, email, created_at, created_by, name, phone) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?"
    -- Note: the edge case of multiple invites to the same team by different admins from the same team results in last-invite-wins in the team_invitation_email table.
    cqlInvitationByEmail :: PrepQuery W (Email, TeamId, InvitationId, InvitationCode, Int32) ()
    cqlInvitationByEmail = "INSERT INTO team_invitation_email (email, team, invitation, code) VALUES (?, ?, ?, ?) USING TTL ?"

lookupInvitation ::
  ( MonadClient m,
    MonadReader Env m,
    Log.MonadLogger m
  ) =>
  ShowOrHideInvitationUrl ->
  TeamId ->
  InvitationId ->
  m (Maybe Invitation)
lookupInvitation showUrl t r = do
  inv <- retry x1 (query1 cqlInvitation (params LocalQuorum (t, r)))
  traverse (toInvitation showUrl) inv
  where
    cqlInvitation :: PrepQuery R (TeamId, InvitationId) (TeamId, Maybe Role, InvitationId, UTCTimeMillis, Maybe UserId, Email, Maybe Name, Maybe Phone, InvitationCode)
    cqlInvitation = "SELECT team, role, id, created_at, created_by, email, name, phone, code FROM team_invitation WHERE team = ? AND id = ?"

lookupInvitationByCode ::
  ( Log.MonadLogger m,
    MonadReader Env m,
    MonadClient m
  ) =>
  ShowOrHideInvitationUrl ->
  InvitationCode ->
  m (Maybe Invitation)
lookupInvitationByCode showUrl i =
  lookupInvitationInfo i >>= \case
    Just InvitationInfo {..} -> lookupInvitation showUrl iiTeam iiInvId
    _ -> pure Nothing

lookupInvitationCode :: (MonadClient m) => TeamId -> InvitationId -> m (Maybe InvitationCode)
lookupInvitationCode t r =
  fmap runIdentity
    <$> retry x1 (query1 cqlInvitationCode (params LocalQuorum (t, r)))
  where
    cqlInvitationCode :: PrepQuery R (TeamId, InvitationId) (Identity InvitationCode)
    cqlInvitationCode = "SELECT code FROM team_invitation WHERE team = ? AND id = ?"

lookupInvitationCodeEmail :: (MonadClient m) => TeamId -> InvitationId -> m (Maybe (InvitationCode, Email))
lookupInvitationCodeEmail t r = retry x1 (query1 cqlInvitationCodeEmail (params LocalQuorum (t, r)))
  where
    cqlInvitationCodeEmail :: PrepQuery R (TeamId, InvitationId) (InvitationCode, Email)
    cqlInvitationCodeEmail = "SELECT code, email FROM team_invitation WHERE team = ? AND id = ?"

lookupInvitations ::
  ( Log.MonadLogger m,
    MonadReader Env m,
    MonadClient m
  ) =>
  ShowOrHideInvitationUrl ->
  TeamId ->
  Maybe InvitationId ->
  Range 1 500 Int32 ->
  m (ResultPage Invitation)
lookupInvitations showUrl team start (fromRange -> size) = do
  page <- case start of
    Just ref -> retry x1 $ paginate cqlSelectFrom (paramsP LocalQuorum (team, ref) (size + 1))
    Nothing -> retry x1 $ paginate cqlSelect (paramsP LocalQuorum (Identity team) (size + 1))
  toResult (hasMore page) <$> traverse (toInvitation showUrl) (trim page)
  where
    trim p = take (fromIntegral size) (result p)
    toResult more invs =
      cassandraResultPage $
        emptyPage
          { result = invs,
            hasMore = more
          }
    cqlSelect :: PrepQuery R (Identity TeamId) (TeamId, Maybe Role, InvitationId, UTCTimeMillis, Maybe UserId, Email, Maybe Name, Maybe Phone, InvitationCode)
    cqlSelect = "SELECT team, role, id, created_at, created_by, email, name, phone, code FROM team_invitation WHERE team = ? ORDER BY id ASC"
    cqlSelectFrom :: PrepQuery R (TeamId, InvitationId) (TeamId, Maybe Role, InvitationId, UTCTimeMillis, Maybe UserId, Email, Maybe Name, Maybe Phone, InvitationCode)
    cqlSelectFrom = "SELECT team, role, id, created_at, created_by, email, name, phone, code FROM team_invitation WHERE team = ? AND id > ? ORDER BY id ASC"

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
    cqlInvitationEmail :: PrepQuery W (Email, TeamId) ()
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

lookupInvitationInfo :: (MonadClient m) => InvitationCode -> m (Maybe InvitationInfo)
lookupInvitationInfo ic@(InvitationCode c)
  | c == mempty = pure Nothing
  | otherwise =
      fmap (toInvitationInfo ic)
        <$> retry x1 (query1 cqlInvitationInfo (params LocalQuorum (Identity ic)))
  where
    toInvitationInfo i (t, r) = InvitationInfo i t r
    cqlInvitationInfo :: PrepQuery R (Identity InvitationCode) (TeamId, InvitationId)
    cqlInvitationInfo = "SELECT team, id FROM team_invitation_info WHERE code = ?"

lookupInvitationByEmail ::
  ( Log.MonadLogger m,
    MonadReader Env m,
    MonadClient m
  ) =>
  ShowOrHideInvitationUrl ->
  Email ->
  m (Maybe Invitation)
lookupInvitationByEmail showUrl e =
  lookupInvitationInfoByEmail e >>= \case
    InvitationByEmail InvitationInfo {..} -> lookupInvitation showUrl iiTeam iiInvId
    _ -> pure Nothing

lookupInvitationInfoByEmail :: (Log.MonadLogger m, MonadClient m) => Email -> m InvitationByEmail
lookupInvitationInfoByEmail email = do
  res <- retry x1 (query cqlInvitationEmail (params LocalQuorum (Identity email)))
  case res of
    [] -> pure InvitationByEmailNotFound
    [(tid, invId, code)] ->
      -- one invite pending
      pure $ InvitationByEmail (InvitationInfo code tid invId)
    _ : _ : _ -> do
      -- edge case: more than one pending invite from different teams
      Log.info $
        Log.msg (Log.val "team_invidation_email: multiple pending invites from different teams for the same email")
          Log.~~ Log.field "email" (show email)
      pure InvitationByEmailMoreThanOne
  where
    cqlInvitationEmail :: PrepQuery R (Identity Email) (TeamId, InvitationId, InvitationCode)
    cqlInvitationEmail = "SELECT team, invitation, code FROM team_invitation_email WHERE email = ?"

countInvitations :: (MonadClient m) => TeamId -> m Int64
countInvitations t =
  maybe 0 runIdentity
    <$> retry x1 (query1 cqlSelect (params LocalQuorum (Identity t)))
  where
    cqlSelect :: PrepQuery R (Identity TeamId) (Identity Int64)
    cqlSelect = "SELECT count(*) FROM team_invitation WHERE team = ?"

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
    Email,
    Maybe Name,
    Maybe Phone,
    InvitationCode
  ) ->
  m Invitation
toInvitation showUrl (t, r, i, tm, minviter, e, inviteeName, p, code) = do
  url <- mkInviteUrl showUrl t code
  pure $ Invitation t (fromMaybe defaultRole r) i tm minviter e inviteeName p url

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
