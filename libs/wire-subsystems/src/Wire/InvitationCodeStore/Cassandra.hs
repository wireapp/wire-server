module Wire.InvitationCodeStore.Cassandra where

import Cassandra
import Data.Id
import Database.CQL.Protocol (TupleType, asRecord)
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.User
import Wire.InvitationCodeStore

interpretInvitationCodeStoreToCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor InvitationCodeStore r
interpretInvitationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      LookupInvitation tid iid -> embed $ lookupInvitationImpl tid iid
      LookupInvitationCodesByEmail email -> embed $ lookupInvitationCodesByEmailImpl email
      LookupInvitationInfoByEmail email -> embed $ lookupInvitationInfoByEmailImp email
      LookupInvitationInfo code -> embed $ lookupInvitationInfoImpl code

lookupInvitationInfoImpl :: InvitationCode -> Client (Maybe InvitationInfo)
lookupInvitationInfoImpl code =
  fmap asRecord <$> retry x1 (query1 cql (params LocalQuorum (Identity code)))
  where
    cql :: PrepQuery R (Identity InvitationCode) (TupleType InvitationInfo)
    cql =
      [sql| 
      SELECT code, team, id FROM team_invitation_info WHERE code = ?
      |]

lookupInvitationCodesByEmailImpl :: EmailAddress -> Client [StoredInvitationByTeam]
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

lookupInvitationInfoByEmailImp :: EmailAddress -> Client InvitationByEmail
lookupInvitationInfoByEmailImp email = do
  res <- retry x1 (query cqlInvitationEmail (params LocalQuorum (Identity email)))
  case res of
    [] -> pure InvitationByEmailNotFound
    [(tid, invId, code)] ->
      -- one invite pending
      pure $ InvitationByEmail (InvitationInfo code tid invId)
    _ : _ : _ -> do
      -- TODO: log the edge case: more than one pending invite from different teams
      -- Log.info $
      --   Log.msg (Log.val "team_invidation_email: multiple pending invites from different teams for the same email")
      --     Log.~~ Log.field "email" (show email)
      pure InvitationByEmailMoreThanOne
  where
    cqlInvitationEmail :: PrepQuery R (Identity EmailAddress) (TeamId, InvitationId, InvitationCode)
    cqlInvitationEmail = "SELECT team, invitation, code FROM team_invitation_email WHERE email = ?"
