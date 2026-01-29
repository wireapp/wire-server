module Wire.SAMLEmailSubsystem.Interpreter
  ( samlEmailSubsystemInterpreter,
  )
where

import Control.Lens ((^..))
import Data.Id (UserId)
import Data.List.NonEmpty qualified as NE
import Data.X509 qualified as X509
import Data.X509.Extended (certDescription)
import Imports
import Polysemy
import SAML2.WebSSO.Types
import Text.Email.Parser
import Wire.API.Locale
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.Member
import Wire.API.User.IdentityProvider
import Wire.EmailSubsystem qualified as Email
import Wire.SAMLEmailSubsystem
import Wire.StoredUser
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

type Receiver = (EmailAddress, Maybe Locale)

sendSAMLIdPChangedImpl ::
  ( Member TeamSubsystem r,
    Member UserStore r,
    Member Email.EmailSubsystem r
  ) =>
  IdpChangedNotification ->
  Sem r ()
sendSAMLIdPChangedImpl notif = mapM_ delegate =<< getReceivers origIdP
  where
    delegate :: (Member Email.EmailSubsystem r) => Receiver -> Sem r ()
    delegate (email, loc) = do
      let endpoint = origIdP._idpMetadata._edRequestURI
          iss = origIdP._idpMetadata._edIssuer
          idPId = origIdP._idpId
          tid = origIdP._idpExtraInfo._team
          (addedCerts, removedCerts) = bimap (certDescription <$>) (certDescription <$>) certsChanges
      Email.sendSAMLIdPChanged email tid mbUserId addedCerts removedCerts idPId iss endpoint loc

    origIdP :: IdP
    origIdP = case notif of
      IdPCreated _userId idp -> idp
      IdPDeleted _userId idp -> idp
      IdPUpdated _userId old _new -> old

    mbUserId :: Maybe UserId
    mbUserId = case notif of
      IdPCreated notifUid _idp -> notifUid
      IdPDeleted notifUid _idp -> Just notifUid
      IdPUpdated notifUid _old _new -> Just notifUid

    certsChanges :: ([X509.SignedCertificate], [X509.SignedCertificate])
    certsChanges = case notif of
      IdPCreated _uid idp -> (toList idp._idpMetadata._edCertAuthnResponse, [])
      IdPDeleted _uid idp -> ([], toList idp._idpMetadata._edCertAuthnResponse)
      IdPUpdated _uid old new ->
        bimap toList toList $
          compareNonEmpty new._idpMetadata._edCertAuthnResponse old._idpMetadata._edCertAuthnResponse

    compareNonEmpty :: (Eq a) => NE.NonEmpty a -> NE.NonEmpty a -> ([a], [a])
    compareNonEmpty xs ys =
      let l = nub . toList $ xs
          r = nub . toList $ ys
          onlyL = l \\ r
          onlyR = r \\ l
       in (onlyL, onlyR)

    getReceivers ::
      ( Member TeamSubsystem r,
        Member UserStore r
      ) =>
      IdP ->
      Sem r [Receiver]
    getReceivers idp = do
      admins <- internalGetTeamAdmins idp._idpExtraInfo._team
      let adminUids = admins ^.. teamMembers . traverse . userId
      catMaybes <$> (toReceiver <$$> getUsers adminUids)
      where
        toReceiver :: StoredUser -> Maybe Receiver
        toReceiver u =
          let loc = flip Locale u.country <$> u.language
           in (,loc) <$> (u.email)
