module Wire.SAMLEmailSubsystem.Interpreter
  ( samlEmailSubsystemInterpreter,
  )
where

import Control.Lens ((^.), (^..))
import Imports
import Polysemy
import SAML2.WebSSO.Types
import Text.Email.Parser
import Wire.API.Team.Member
import Wire.API.User.IdentityProvider (IdP, team)
import Wire.EmailSubsystem qualified as Email
import Wire.SAMLEmailSubsystem
import Wire.TeamSubsystem
import Wire.UserStore

samlEmailSubsystemInterpreter ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  InterpreterFor SAMLEmailSubsystem r
samlEmailSubsystemInterpreter = interpret \case
  SendSAMLIdPCreated idp -> sendSAMLIdPCreatedImpl idp
  SendSAMLIdPDeleted idp -> sendSAMLIdPDeletedImpl idp
  SendSAMLIdPUpdated old new -> sendSAMLIdPUpdatedImpl old new

sendSAMLIdPCreatedImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  IdP ->
  Sem r ()
sendSAMLIdPCreatedImpl idp = do
  emails <- getEmailAddresses idp
  mapM_ (Email.sendSAMLIdPCreated idp) emails

sendSAMLIdPDeletedImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  IdP ->
  Sem r ()
sendSAMLIdPDeletedImpl idp = do
  emails <- getEmailAddresses idp
  mapM_ (Email.sendSAMLIdPDeleted idp) emails

sendSAMLIdPUpdatedImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  IdP ->
  IdP ->
  Sem r ()
sendSAMLIdPUpdatedImpl old new = do
  emails <- getEmailAddresses old
  mapM_ (Email.sendSAMLIdPUpdated old new) emails

getEmailAddresses ::
  ( Member TeamSubsystem r,
    Member UserStore r
  ) =>
  IdP ->
  Sem r [EmailAddress]
getEmailAddresses idp = do
  admins <- internalGetTeamAdmins (idp ^. idpExtraInfo . team)
  let adminUids = admins ^.. teamMembers . traverse . userId
  getEmails adminUids
