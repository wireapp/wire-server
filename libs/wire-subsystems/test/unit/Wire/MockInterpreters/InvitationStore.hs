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

module Wire.MockInterpreters.InvitationStore where

import Data.Id (InvitationId, TeamId, UserId)
import Data.Map (alter, elems, (!?))
import Data.Map qualified as M
import Data.Range (fromRange)
import Data.Time (UTCTime)
import Imports hiding ((!?))
import Polysemy
import Polysemy.State (State, get, gets, modify)
import Wire.API.User (EmailAddress, InvitationCode (..))
import Wire.InvitationStore
import Wire.InvitationStore qualified as Stored (StoredInvitation (role))
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now

-- | Mock counterpart of @team_invitation.expires_at@ (Postgres) and of the
-- row TTL (Cassandra).  Invitations without an entry never expire; 'InsertInvitation'
-- does not add entries, so tests opt into expiry by seeding this map.
type InvitationExpiries = Map (TeamId, InvitationId) UTCTime

isLiveInvitation :: UTCTime -> InvitationExpiries -> StoredInvitation -> Bool
isLiveInvitation now expiries inv =
  maybe True (> now) $ expiries !? (inv.teamId, inv.invitationId)

inMemoryInvitationStoreInterpreter ::
  forall r.
  ( Member (State (Map (TeamId, InvitationId) StoredInvitation)) r,
    Member (State (Map (InvitationCode) StoredInvitation)) r,
    Member (State (Map (TeamId, EmailAddress) [UserId])) r,
    Member (State InvitationExpiries) r,
    Member Now r
  ) =>
  InterpreterFor InvitationStore r
inMemoryInvitationStoreInterpreter = interpret \case
  InsertInvitation inv _timeout ->
    let -- NB: timeout is not taken into account, see 'InvitationExpiries'.
        modByIds :: Map (TeamId, InvitationId) StoredInvitation -> Map (TeamId, InvitationId) StoredInvitation
        modByIds = alter (\_ -> Just $ insertInvToStoredInv inv) (inv.teamId, inv.invitationId)

        modByCode :: Map (InvitationCode) StoredInvitation -> Map (InvitationCode) StoredInvitation
        modByCode = alter (\_ -> Just $ insertInvToStoredInv inv) inv.code
     in do
          modify modByIds
          modify modByCode
          fromJust <$> gets (!? inv.code)
  LookupInvitation tid iid -> onlyLiveMaybe =<< gets (!? (tid, iid))
  UpdateInvitationRole tid iid role -> do
    inv <- onlyLiveMaybe =<< gets @(Map (TeamId, InvitationId) StoredInvitation) (!? (tid, iid))
    for_ inv $ \i -> do
      let updated = i {Stored.role = Just role}
      modify @(Map (TeamId, InvitationId) StoredInvitation) (M.insert (tid, iid) updated)
      modify @(Map InvitationCode StoredInvitation) (M.insert i.code updated)
  LookupInvitationByCode iid -> onlyLiveMaybe =<< gets (!? iid)
  LookupInvitationsByEmail em ->
    let c i = guard (i.email == em) $> i
     in onlyLive . mapMaybe c . elems =<< get @(Map (TeamId, InvitationId) _)
  InsertPendingScimUser tid email uid ->
    modify @(Map (TeamId, EmailAddress) [UserId]) (M.insertWith (++) (tid, email) [uid])
  LookupPendingScimUsers tid email ->
    gets @(Map (TeamId, EmailAddress) [UserId]) (fromMaybe [] . (!? (tid, email)))
  DeletePendingScimUser tid email uid ->
    modify @(Map (TeamId, EmailAddress) [UserId]) $
      M.alter
        ( \case
            Nothing -> Nothing
            Just uids -> case filter (/= uid) uids of
              [] -> Nothing
              remaining -> Just remaining
        )
        (tid, email)
  LookupInvitationsPaginated mSize tid cursor -> do
    invitations <- onlyLive =<< gets @(Map (TeamId, InvitationId) StoredInvitation) elems
    let matching = sortOn (.invitationId) $ filter (\inv -> inv.teamId == tid && maybe True (< inv.invitationId) cursor) invitations
        size = fromIntegral $ maybe 100 fromRange mSize
        page = take size matching
    pure $ if length matching > size then PaginatedResultHasMore page else PaginatedResult page
  CountInvitations tid -> do
    invitations <- onlyLive =<< gets @(Map (TeamId, InvitationId) StoredInvitation) elems
    pure . fromIntegral . length $ filter (\inv -> inv.teamId == tid) invitations
  DeleteInvitation _tid _invId -> error "DeleteInvitation"
  DeleteAllTeamInvitations _tid -> error "DeleteAllTeamInvitations"
  where
    onlyLive :: [StoredInvitation] -> Sem r [StoredInvitation]
    onlyLive invs = do
      now <- Now.get
      expiries <- get @InvitationExpiries
      pure $ filter (isLiveInvitation now expiries) invs

    onlyLiveMaybe :: Maybe StoredInvitation -> Sem r (Maybe StoredInvitation)
    onlyLiveMaybe = fmap listToMaybe . onlyLive . maybeToList
