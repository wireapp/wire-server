module Wire.InvitationCodeStore.Cassandra
  ( interpretInvitationCodeStoreToCassandra,
  )
where

import Cassandra
import Control.Monad.Trans.Maybe
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as Conduit
import Data.Id
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Range (Range, fromRange)
import Database.CQL.Protocol (Record (..), TupleType, asRecord)
import Imports
import Polysemy
import Polysemy.Embed
import UnliftIO.Async (pooledMapConcurrentlyN_)
import Util.Timeout
import Wire.API.Team.Role (Role)
import Wire.API.User
import Wire.InvitationCodeStore

interpretInvitationCodeStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor InvitationCodeStore r
interpretInvitationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      InsertInvitation newInv timeout -> embed $ insertInvitationImpl newInv timeout
      LookupInvitation tid iid -> embed $ lookupInvitationImpl tid iid
      LookupInvitationCodesByEmail email -> embed $ lookupInvitationCodesByEmailImpl email
      LookupInvitationByCode code -> embed $ lookupInvitationByCodeImpl code
      LookupInvitationsPaginated mSize tid miid -> embed $ lookupInvitationsPaginatedImpl mSize tid miid
      CountInvitations tid -> embed $ countInvitationsImpl tid
      DeleteInvitation tid invId -> embed $ deleteInvitationImpl tid invId
      DeleteAllTeamInvitations tid -> embed $ deleteInvitationsImpl tid

insertInvitationImpl ::
  InsertInvitation ->
  -- | The timeout for the invitation code.
  Timeout ->
  Client StoredInvitation
insertInvitationImpl (MkInsertInvitation invId teamId role (toUTCTimeMillis -> now) uid email name code) timeout = do
  let inv =
        MkStoredInvitation
          { teamId = teamId,
            role = Just role,
            invitationId = invId,
            createdAt = now,
            createdBy = uid,
            email = email,
            name = name,
            code = code
          }
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery cqlInsert (teamId, Just role, invId, now, uid, email, name, code, round timeout)
    addPrepQuery cqlInsertInfo (code, teamId, invId, round timeout)
    addPrepQuery cqlInsertByEmail (email, teamId, invId, code, round timeout)
  pure inv
  where
    cqlInsert :: PrepQuery W (TeamId, Maybe Role, InvitationId, UTCTimeMillis, Maybe UserId, EmailAddress, Maybe Name, InvitationCode, Int32) ()
    cqlInsert =
      [sql|
        INSERT INTO team_invitation (team, role, id, created_at, created_by, email, name, code) VALUES (?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?
      |]
    cqlInsertInfo :: PrepQuery W (InvitationCode, TeamId, InvitationId, Int32) ()
    cqlInsertInfo =
      [sql|
            INSERT INTO team_invitation_info (code, team, id) VALUES (?, ?, ?) USING TTL ?
          |]
    -- Note: the edge case of multiple invites to the same team by different admins from the
    -- same team results in last-invite-wins in the team_invitation_email table.
    cqlInsertByEmail :: PrepQuery W (EmailAddress, TeamId, InvitationId, InvitationCode, Int32) ()
    cqlInsertByEmail =
      [sql|
            INSERT INTO team_invitation_email (email, team, invitation, code) VALUES (?, ?, ?, ?) USING TTL ?
          |]

lookupInvitationsPaginatedImpl :: Maybe (Range 1 500 Int32) -> TeamId -> Maybe InvitationId -> Client (PaginatedResult [StoredInvitation])
lookupInvitationsPaginatedImpl mSize tid miid = do
  page <- retry x1 case miid of
    Just ref -> paginate cqlSelectFrom (paramsP LocalQuorum (tid, ref) (pageSize + 1))
    Nothing -> paginate cqlSelect (paramsP LocalQuorum (Identity tid) (pageSize + 1))
  pure $ mkPage (hasMore page) $ map asRecord $ trim page
  where
    pageSize :: Int32
    pageSize = maybe 100 fromRange mSize

    trim :: Page a -> [a]
    trim p = take (fromIntegral pageSize) (result p)

    mkPage more invs = if more then PaginatedResultHasMore invs else PaginatedResult invs

    cqlSelect :: PrepQuery R (Identity TeamId) (TeamId, Maybe Role, InvitationId, UTCTimeMillis, Maybe UserId, EmailAddress, Maybe Name, InvitationCode)
    cqlSelect =
      [sql|
      SELECT team, role, id, created_at, created_by, email, name, code FROM team_invitation WHERE team = ? ORDER BY id ASC
      |]
    cqlSelectFrom :: PrepQuery R (TeamId, InvitationId) (TeamId, Maybe Role, InvitationId, UTCTimeMillis, Maybe UserId, EmailAddress, Maybe Name, InvitationCode)
    cqlSelectFrom =
      [sql|
      SELECT team, role, id, created_at, created_by, email, name, code FROM team_invitation WHERE team = ? AND id > ? ORDER BY id ASC
      |]

countInvitationsImpl :: TeamId -> Client (Int64)
countInvitationsImpl t =
  maybe 0 runIdentity
    <$> retry x1 (query1 cql (params LocalQuorum (Identity t)))
  where
    cql :: PrepQuery R (Identity TeamId) (Identity Int64)
    cql = [sql| SELECT count(*) FROM team_invitation WHERE team = ?|]

lookupInvitationByCodeImpl :: InvitationCode -> Client (Maybe StoredInvitation)
lookupInvitationByCodeImpl code = runMaybeT do
  (teamId, invId, _) <-
    MaybeT $
      retry x1 (query1 cqlInfo (params LocalQuorum (Identity code)))
  MaybeT $ fmap asRecord <$> retry x1 (query1 cqlMain (params LocalQuorum (teamId, invId)))
  where
    cqlInfo :: PrepQuery R (Identity InvitationCode) (TeamId, InvitationId, InvitationCode)
    cqlInfo =
      [sql|
      SELECT team, id, code FROM team_invitation_info WHERE code = ?
      |]
    cqlMain :: PrepQuery R (TeamId, InvitationId) (TupleType StoredInvitation)
    cqlMain =
      [sql|
      SELECT team, role, id, created_at, created_by, email, name, code FROM team_invitation WHERE team = ? AND id = ?
      |]

lookupInvitationCodesByEmailImpl :: EmailAddress -> Client [StoredInvitation]
lookupInvitationCodesByEmailImpl email = do
  infoList <-
    retry x1 (query cqlInfo (params LocalQuorum (Identity email)))
  fmap catMaybes $ forM infoList $ \(tid, invId, _invCode) ->
    fmap asRecord <$> retry x1 (query1 cqlMain (params LocalQuorum (tid, invId)))
  where
    cqlInfo :: PrepQuery R (Identity EmailAddress) (TeamId, InvitationId, InvitationCode)
    cqlInfo =
      [sql|
      SELECT team, invitation, code FROM team_invitation_email WHERE email = ?
      |]
    cqlMain :: PrepQuery R (TeamId, InvitationId) (TupleType StoredInvitation)
    cqlMain =
      [sql|
      SELECT team, role, id, created_at, created_by, email, name, code FROM team_invitation WHERE team = ? AND id = ?
      |]

lookupInvitationImpl :: TeamId -> InvitationId -> Client (Maybe StoredInvitation)
lookupInvitationImpl tid iid =
  fmap asRecord
    <$> retry x1 (query1 cql (params LocalQuorum (tid, iid)))
  where
    cql :: PrepQuery R (TeamId, InvitationId) (TupleType StoredInvitation)
    cql =
      [sql|
      SELECT team, role, id, created_at, created_by, email, name, code FROM team_invitation WHERE team = ? AND id = ?
      |]

deleteInvitationImpl :: TeamId -> InvitationId -> Client ()
deleteInvitationImpl teamId invId = do
  codeEmail <- lookupInvitationCodeEmail
  case codeEmail of
    Just (invCode, invEmail) -> retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      addPrepQuery cqlInvitation (teamId, invId)
      addPrepQuery cqlInvitationInfo (Identity invCode)
      addPrepQuery cqlInvitationEmail (invEmail, teamId)
    Nothing ->
      retry x5 $ write cqlInvitation (params LocalQuorum (teamId, invId))
  where
    lookupInvitationCodeEmail :: Client (Maybe (InvitationCode, EmailAddress))
    lookupInvitationCodeEmail = retry x1 (query1 cqlInvitationCodeEmail (params LocalQuorum (teamId, invId)))

    cqlInvitation :: PrepQuery W (TeamId, InvitationId) ()
    cqlInvitation =
      [sql|
        DELETE FROM team_invitation where team = ? AND id = ?
      |]

    cqlInvitationInfo :: PrepQuery W (Identity InvitationCode) ()
    cqlInvitationInfo =
      [sql|
        DELETE FROM team_invitation_info WHERE code = ?
      |]

    cqlInvitationEmail :: PrepQuery W (EmailAddress, TeamId) ()
    cqlInvitationEmail =
      [sql|
        DELETE FROM team_invitation_email WHERE email = ? AND team = ?
      |]

    cqlInvitationCodeEmail :: PrepQuery R (TeamId, InvitationId) (InvitationCode, EmailAddress)
    cqlInvitationCodeEmail =
      [sql|
        SELECT code, email FROM team_invitation WHERE team = ? AND id = ?
      |]

deleteInvitationsImpl :: TeamId -> Client ()
deleteInvitationsImpl teamId =
  runConduit $
    paginateC cqlSelect (paramsP LocalQuorum (Identity teamId) 100) x1
      .| Conduit.mapM_ (pooledMapConcurrentlyN_ 16 (deleteInvitationImpl teamId . runIdentity))
  where
    cqlSelect :: PrepQuery R (Identity TeamId) (Identity InvitationId)
    cqlSelect = "SELECT id FROM team_invitation WHERE team = ? ORDER BY id ASC"
