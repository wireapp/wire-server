module Wire.InvitationCodeStore.Cassandra where

import Cassandra
import Data.Id
import Database.CQL.Protocol (TupleType, asRecord)
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.User
import Wire.InvitationCodeStore

interpretInvitationCodeStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor InvitationCodeStore r
interpretInvitationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      LookupInvitation tid iid -> embed $ lookupInvitationImpl tid iid
      LookupInvitationCodesByEmail email -> embed $ lookupInvitationCodesByEmailImpl email
      LookupInvitationInfo code -> embed $ lookupInvitationInfoImpl code

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
