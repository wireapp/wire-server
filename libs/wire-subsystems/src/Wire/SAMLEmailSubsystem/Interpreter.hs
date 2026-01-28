module Wire.SAMLEmailSubsystem.Interpreter
  ( samlEmailSubsystemInterpreter,
  )
where

import Control.Lens ((^.), (^..))
import Data.Id (UserId)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.X509 qualified as X509
import Data.X509.Extended (CertDescription (..), certDescription)
import Imports
import Polysemy
import SAML2.WebSSO.Types
import Text.Email.Parser
import Wire.API.Locale
import Wire.API.Routes.Internal.Brig
import Wire.API.Team.Member
import Wire.API.User.IdentityProvider
import Wire.EmailSubsystem (IdPDetails (..))
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
sendSAMLIdPChangedImpl notif = do
  receivers <- getReceivers origIdP
  mapM_ delegate receivers
  where
    delegate :: (Member Email.EmailSubsystem r) => Receiver -> Sem r ()
    delegate (email, loc) = do
      let endpoint = origIdP._idpMetadata._edRequestURI
          iss = origIdP._idpMetadata._edIssuer
          idPId = origIdP._idpId
          tid = origIdP ^. idpExtraInfo . team
          (addedCerts, removedCerts) = bimap (toDesc <$>) (toDesc <$>) certsChanges
      Email.sendSAMLIdPChanged email tid mbUserId addedCerts removedCerts idPId iss endpoint loc

    origIdP :: IdP
    origIdP = case notif of
      IdPCreated _userId idp -> idp
      IdPDeleted _userId idp -> idp
      IdPUpdated _userId old _new -> old

    mbUserId :: Maybe UserId
    mbUserId = case notif of
      IdPCreated nofifUid _idp -> nofifUid
      IdPDeleted nofifUid _idp -> Just nofifUid
      IdPUpdated nofifUid _old _new -> Just nofifUid

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

    toDesc :: X509.SignedCertificate -> IdPDetails
    toDesc cert =
      let desc = certDescription cert
       in IdPDetails
            { idpDescriptionFingerprintAlgorithm = T.pack desc.fingerprintAlgorithm,
              idpDescriptionFingerprint = T.pack desc.fingerprint,
              idpDescriptionSubject = T.pack desc.subject
            }

getReceivers ::
  ( Member TeamSubsystem r,
    Member UserStore r
  ) =>
  IdP ->
  Sem r [Receiver]
getReceivers idp = do
  -- TODO: Replace lens
  admins <- internalGetTeamAdmins (idp ^. idpExtraInfo . team)
  let adminUids = admins ^.. teamMembers . traverse . userId
  catMaybes <$> (toReceiver <$$> getUsers adminUids)
  where
    toReceiver :: StoredUser -> Maybe Receiver
    toReceiver u =
      let loc = flip Locale u.country <$> u.language
       in (,loc) <$> (u.email)
