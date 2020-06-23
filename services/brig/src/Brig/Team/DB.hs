{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    InvitationInfo (..),
    InvitationByEmail (..),
  )
where

import Brig.Data.Instances ()
import Brig.Data.Types as T
import Brig.Options
import Brig.Types.Common
import Brig.Types.Team.Invitation
import Brig.Types.User
import Cassandra as C
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.List as C
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Range
import Data.Text.Ascii (encodeBase64Url)
import Data.Time.Clock
import Data.UUID.V4
import Galley.Types.Teams (Role)
import qualified Galley.Types.Teams as Team
import Imports
import OpenSSL.Random (randBytes)
import qualified System.Logger.Class as Log
import UnliftIO.Async (pooledMapConcurrentlyN_)

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
  MonadClient m =>
  TeamId ->
  Role ->
  Email ->
  UTCTime ->
  Maybe UserId ->
  Maybe Name ->
  Maybe Phone ->
  -- | The timeout for the invitation code.
  Timeout ->
  m (Invitation, InvitationCode)
insertInvitation t role email (toUTCTimeMillis -> now) minviter inviteeName phone timeout = do
  iid <- liftIO mkInvitationId
  code <- liftIO mkInvitationCode
  let inv = Invitation t role iid email now minviter inviteeName phone
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery cqlInvitation (t, role, iid, code, email, now, minviter, inviteeName, phone, round timeout)
    addPrepQuery cqlInvitationInfo (code, t, iid, round timeout)
    addPrepQuery cqlInvitationByEmail (email, t, iid, code, round timeout)
  return (inv, code)
  where
    cqlInvitationInfo :: PrepQuery W (InvitationCode, TeamId, InvitationId, Int32) ()
    cqlInvitationInfo = "INSERT INTO team_invitation_info (code, team, id) VALUES (?, ?, ?) USING TTL ?"
    cqlInvitation :: PrepQuery W (TeamId, Role, InvitationId, InvitationCode, Email, UTCTimeMillis, Maybe UserId, Maybe Name, Maybe Phone, Int32) ()
    cqlInvitation = "INSERT INTO team_invitation (team, role, id, code, email, created_at, created_by, name, phone) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?"
    -- Note: the edge case of multiple invites to the same team by different admins from the same team results in last-invite-wins in the team_invitation_email table.
    cqlInvitationByEmail :: PrepQuery W (Email, TeamId, InvitationId, InvitationCode, Int32) ()
    cqlInvitationByEmail = "INSERT INTO team_invitation_email (email, team, invitation, code) VALUES (?, ?, ?, ?) USING TTL ?"

lookupInvitation :: MonadClient m => TeamId -> InvitationId -> m (Maybe Invitation)
lookupInvitation t r =
  fmap toInvitation
    <$> retry x1 (query1 cqlInvitation (params Quorum (t, r)))
  where
    cqlInvitation :: PrepQuery R (TeamId, InvitationId) (TeamId, Maybe Role, InvitationId, Email, UTCTimeMillis, Maybe UserId, Maybe Name, Maybe Phone)
    cqlInvitation = "SELECT team, role, id, email, created_at, created_by, name, phone FROM team_invitation WHERE team = ? AND id = ?"

lookupInvitationByCode :: MonadClient m => InvitationCode -> m (Maybe Invitation)
lookupInvitationByCode i = lookupInvitationInfo i >>= \case
  Just InvitationInfo {..} -> lookupInvitation iiTeam iiInvId
  _ -> return Nothing

lookupInvitationCode :: MonadClient m => TeamId -> InvitationId -> m (Maybe InvitationCode)
lookupInvitationCode t r =
  fmap runIdentity
    <$> retry x1 (query1 cqlInvitationCode (params Quorum (t, r)))
  where
    cqlInvitationCode :: PrepQuery R (TeamId, InvitationId) (Identity InvitationCode)
    cqlInvitationCode = "SELECT code FROM team_invitation WHERE team = ? AND id = ?"

lookupInvitationCodeEmail :: MonadClient m => TeamId -> InvitationId -> m (Maybe (InvitationCode, Email))
lookupInvitationCodeEmail t r = retry x1 (query1 cqlInvitationCodeEmail (params Quorum (t, r)))
  where
    cqlInvitationCodeEmail :: PrepQuery R (TeamId, InvitationId) (InvitationCode, Email)
    cqlInvitationCodeEmail = "SELECT code, email FROM team_invitation WHERE team = ? AND id = ?"

lookupInvitations :: MonadClient m => TeamId -> Maybe InvitationId -> Range 1 500 Int32 -> m (ResultPage Invitation)
lookupInvitations team start (fromRange -> size) = do
  page <- case start of
    Just ref -> retry x1 $ paginate cqlSelectFrom (paramsP Quorum (team, ref) (size + 1))
    Nothing -> retry x1 $ paginate cqlSelect (paramsP Quorum (Identity team) (size + 1))
  return $ toResult (hasMore page) $ map toInvitation (trim page)
  where
    trim p = take (fromIntegral size) (result p)
    toResult more invs =
      cassandraResultPage $
        emptyPage
          { result = invs,
            hasMore = more
          }
    cqlSelect :: PrepQuery R (Identity TeamId) (TeamId, Maybe Role, InvitationId, Email, UTCTimeMillis, Maybe UserId, Maybe Name, Maybe Phone)
    cqlSelect = "SELECT team, role, id, email, created_at, created_by, name, phone FROM team_invitation WHERE team = ? ORDER BY id ASC"
    cqlSelectFrom :: PrepQuery R (TeamId, InvitationId) (TeamId, Maybe Role, InvitationId, Email, UTCTimeMillis, Maybe UserId, Maybe Name, Maybe Phone)
    cqlSelectFrom = "SELECT team, role, id, email, created_at, created_by, name, phone FROM team_invitation WHERE team = ? AND id > ? ORDER BY id ASC"

deleteInvitation :: MonadClient m => TeamId -> InvitationId -> m ()
deleteInvitation t i = do
  codeEmail <- lookupInvitationCodeEmail t i
  case codeEmail of
    Just (invCode, invEmail) -> retry x5 . batch $ do
      setType BatchLogged
      setConsistency Quorum
      addPrepQuery cqlInvitation (t, i)
      addPrepQuery cqlInvitationInfo (Identity invCode)
      addPrepQuery cqlInvitationEmail (invEmail, t)
    Nothing ->
      retry x5 $ write cqlInvitation (params Quorum (t, i))
  where
    cqlInvitation :: PrepQuery W (TeamId, InvitationId) ()
    cqlInvitation = "DELETE FROM team_invitation where team = ? AND id = ?"
    cqlInvitationInfo :: PrepQuery W (Identity InvitationCode) ()
    cqlInvitationInfo = "DELETE FROM team_invitation_info WHERE code = ?"
    cqlInvitationEmail :: PrepQuery W (Email, TeamId) ()
    cqlInvitationEmail = "DELETE FROM team_invitation_email WHERE email = ? AND team = ?"

deleteInvitations :: (MonadClient m, MonadUnliftIO m) => TeamId -> m ()
deleteInvitations t =
  liftClient
    $ runConduit
    $ paginateC cqlSelect (paramsP Quorum (Identity t) 100) x1
      .| C.mapM_ (pooledMapConcurrentlyN_ 16 (deleteInvitation t . runIdentity))
  where
    cqlSelect :: PrepQuery R (Identity TeamId) (Identity InvitationId)
    cqlSelect = "SELECT id FROM team_invitation WHERE team = ? ORDER BY id ASC"

lookupInvitationInfo :: MonadClient m => InvitationCode -> m (Maybe InvitationInfo)
lookupInvitationInfo ic@(InvitationCode c)
  | c == mempty = return Nothing
  | otherwise =
    fmap (toInvitationInfo ic)
      <$> retry x1 (query1 cqlInvitationInfo (params Quorum (Identity ic)))
  where
    toInvitationInfo i (t, r) = InvitationInfo i t r
    cqlInvitationInfo :: PrepQuery R (Identity InvitationCode) (TeamId, InvitationId)
    cqlInvitationInfo = "SELECT team, id FROM team_invitation_info WHERE code = ?"

lookupInvitationByEmail :: (Log.MonadLogger m, MonadClient m) => Email -> m (Maybe Invitation)
lookupInvitationByEmail e = lookupInvitationInfoByEmail e >>= \case
  InvitationByEmail InvitationInfo {..} -> lookupInvitation iiTeam iiInvId
  _ -> return Nothing

lookupInvitationInfoByEmail :: (Log.MonadLogger m, MonadClient m) => Email -> m InvitationByEmail
lookupInvitationInfoByEmail email = do
  res <- retry x1 (query cqlInvitationEmail (params Quorum (Identity email)))
  case res of
    [] -> return InvitationByEmailNotFound
    (tid, invId, code) : [] ->
      -- one invite pending
      return $ InvitationByEmail (InvitationInfo code tid invId)
    _ : _ : _ -> do
      -- edge case: more than one pending invite from different teams
      Log.info $
        Log.msg (Log.val "team_invidation_email: multiple pending invites from different teams for the same email")
          Log.~~ Log.field "email" (show email)
      return InvitationByEmailMoreThanOne
  where
    cqlInvitationEmail :: PrepQuery R (Identity Email) (TeamId, InvitationId, InvitationCode)
    cqlInvitationEmail = "SELECT team, invitation, code FROM team_invitation_email WHERE email = ?"

countInvitations :: MonadClient m => TeamId -> m Int64
countInvitations t =
  fromMaybe 0 . fmap runIdentity
    <$> retry x1 (query1 cqlSelect (params Quorum (Identity t)))
  where
    cqlSelect :: PrepQuery R (Identity TeamId) (Identity Int64)
    cqlSelect = "SELECT count(*) FROM team_invitation WHERE team = ?"

-- | brig used to not store the role, so for migration we allow this to be empty and fill in the
-- default here.
toInvitation :: (TeamId, Maybe Role, InvitationId, Email, UTCTimeMillis, Maybe UserId, Maybe Name, Maybe Phone) -> Invitation
toInvitation (t, r, i, e, tm, minviter, inviteeName, p) = Invitation t (fromMaybe Team.defaultRole r) i e tm minviter inviteeName p
