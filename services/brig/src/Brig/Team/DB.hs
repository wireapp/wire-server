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
    deleteInvitation,
    deleteInvitations,
    mkInvitationCode,
    mkInvitationId,
  )
where

import Brig.Data.Types as T
import Cassandra as C
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as C
import Data.Id
import Data.Text.Ascii (encodeBase64Url)
import Data.UUID.V4
import Imports
import OpenSSL.Random (randBytes)
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Wire.API.User

mkInvitationCode :: IO InvitationCode
mkInvitationCode = InvitationCode . encodeBase64Url <$> randBytes 24

mkInvitationId :: IO InvitationId
mkInvitationId = Id <$> nextRandom

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
