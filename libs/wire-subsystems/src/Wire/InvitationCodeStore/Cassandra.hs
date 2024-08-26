module Wire.InvitationCodeStore.Cassandra where

import Cassandra
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Range (Range, fromRange)
import Database.CQL.Protocol (TupleType, asRecord)
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.Team.Role (Role)
import Wire.API.User
import Wire.InvitationCodeStore

interpretInvitationCodeStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor InvitationCodeStore r
interpretInvitationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      LookupInvitation tid iid -> embed $ lookupInvitationImpl tid iid
      LookupInvitationCodesByEmail email -> embed $ lookupInvitationCodesByEmailImpl email
      LookupInvitationInfo code -> embed $ lookupInvitationInfoImpl code
      CountInvitations tid -> embed $ countInvitationsImpl tid
      LookupInvitationsPaginated mSize tid miid -> embed $ lookupInvitationsPaginatedImpl mSize tid miid

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
    cql = [sql|  count(*) FROM team_invitation WHERE team = ?|]

lookupInvitationInfoImpl :: InvitationCode -> Client (Maybe StoredInvitationInfo)
lookupInvitationInfoImpl code =
  fmap asRecord <$> retry x1 (query1 cql (params LocalQuorum (Identity code)))
  where
    cql :: PrepQuery R (Identity InvitationCode) (TupleType StoredInvitationInfo)
    cql =
      [sql| 
      SELECT  team, id, code FROM team_invitation_info WHERE code = ?
      |]

lookupInvitationCodesByEmailImpl :: EmailAddress -> Client [StoredInvitationInfo]
lookupInvitationCodesByEmailImpl email = map asRecord <$> retry x1 (query cql (params LocalQuorum (Identity email)))
  where
    cql :: PrepQuery R (Identity EmailAddress) (TeamId, InvitationId, InvitationCode)
    cql =
      [sql| 
      SELECT team, invitation, code FROM team_invitation_email WHERE email = ?
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
