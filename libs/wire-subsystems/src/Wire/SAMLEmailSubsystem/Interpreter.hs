module Wire.SAMLEmailSubsystem.Interpreter
  ( samlEmailSubsystemInterpreter,
  )
where

import Control.Lens ((^.), (^..))
import Imports
import Polysemy
import SAML2.WebSSO.Types
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

sendSAMLIdPCreatedImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  IdP ->
  Sem r ()
sendSAMLIdPCreatedImpl idp = do
  admins <- internalGetTeamAdmins (idp ^. idpExtraInfo . team)
  let adminUids = admins ^.. teamMembers . traverse . userId
  emails <- getEmails adminUids
  mapM_ (flip Email.sendSAMLIdPCreated idp) emails
