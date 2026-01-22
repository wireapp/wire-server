module Wire.SAMLEmailSubsystem.Interpreter
  ( samlEmailSubsystemInterpreter,
  )
where

import Control.Lens ((^.), (^..))
import Imports
import Polysemy
import SAML2.WebSSO.Types
import Text.Email.Parser
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.Member
import Wire.API.User.IdentityProvider
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
  SendSAMLIdPChanged idp -> sendSAMLIdPChangedImpl idp

sendSAMLIdPChangedImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  IdpChangedNotification ->
  Sem r ()
sendSAMLIdPChangedImpl notif = do
  emails <- getEmailAddresses origIdP
  mapM_ delegate emails
  where
    delegate :: (Member Email.EmailSubsystem r) => EmailAddress -> Sem r ()
    delegate email = case notif of
      IdPCreated _userId idp -> Email.sendSAMLIdPCreated idp email
      IdPDeleted _userId idp -> Email.sendSAMLIdPDeleted idp email
      IdPUpdated _userId old new -> Email.sendSAMLIdPUpdated old new email

    origIdP :: IdP
    origIdP = case notif of
      IdPCreated _userId idp -> idp
      IdPDeleted _userId idp -> idp
      IdPUpdated _userId old _new -> old

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
