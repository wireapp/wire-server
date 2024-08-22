module Wire.InvitationCodeStore.Cassandra where

import Cassandra
import Data.Id
import Database.CQL.Protocol (TupleType, asRecord)
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.TinyLog (TinyLog)
import System.Logger.Message qualified as Log
import Wire.API.User
import Wire.InvitationCodeStore
import Wire.Sem.Logger qualified as Log

interpretInvitationCodeStoreToCassandra :: (Member (Embed IO) r, Member TinyLog r) => ClientState -> InterpreterFor InvitationCodeStore r
interpretInvitationCodeStoreToCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      LookupInvitation tid iid -> embed $ lookupInvitationImpl tid iid
      LookupInvitationCodesByEmail email -> embed $ lookupInvitationCodesByEmailImpl email
      LookupSingleInvitationCodeByEmail email -> lookupSingleInvitationCodeByEmailImpl email

lookupSingleInvitationCodeByEmailImpl :: (Member TinyLog r) => Email -> Sem (Embed Client : r) (Maybe StoredInvitationByTeam)
lookupSingleInvitationCodeByEmailImpl email = do
  invs <- embed $ lookupInvitationCodesByEmailImpl email
  case invs of
    [] -> pure Nothing
    [inv] -> pure $ Just inv
    (_ : _ : _) -> do
      -- edge case: more than one pending invite from different teams
      Log.info $
        Log.msg (Log.val "team_invidation_email: multiple pending invites from different teams for the same email")
          . Log.field "email" (show email)

      pure Nothing

lookupInvitationCodesByEmailImpl :: Email -> Client [StoredInvitationByTeam]
lookupInvitationCodesByEmailImpl email = map asRecord <$> retry x1 (query cql (params LocalQuorum (Identity email)))
  where
    cql :: PrepQuery R (Identity Email) (TeamId, InvitationId, InvitationCode)
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
