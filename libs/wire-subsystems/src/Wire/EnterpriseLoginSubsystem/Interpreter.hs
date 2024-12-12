{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.EnterpriseLoginSubsystem.Interpreter
  ( runEnterpriseLoginSubsystem,
    EnterpriseLoginSubsystemConfig (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain, domainText)
import Data.Id
import Data.Misc (HttpsUrl (..))
import Data.Text.Internal.Builder (fromLazyText, fromText, toLazyText)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Imports hiding (lookup)
import Network.Mail.Mime (Address (Address), Mail (mailHeaders, mailParts, mailTo), emptyMail, plainPart)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Input (Input, input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Wire.API.EnterpriseLogin
import Wire.API.User.EmailAddress (EmailAddress, fromEmail)
import Wire.DomainRegistrationStore
import Wire.EmailSending (EmailSending, sendMail)
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error

data EnterpriseLoginSubsystemConfig = EnterpriseLoginSubsystemConfig
  { auditEmailSender :: EmailAddress,
    auditEmailRecipient :: EmailAddress
  }

runEnterpriseLoginSubsystem ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Sem (EnterpriseLoginSubsystem ': r) a ->
  Sem r a
runEnterpriseLoginSubsystem = interpret $
  \case
    LockDomain domain -> lockDomainImpl domain
    UnlockDomain domain -> unlockDomainImpl domain
    PreAuthorizeDomain domain -> preAuthorizeImpl domain
    UnAuthorizeDomain domain -> unauthorizeImpl domain
    UpdateDomainRegistration domain update -> updateDomainRegistrationImpl domain update
    DeleteDomain domain -> deleteDomainImpl domain
    GetDomainRegistration domain -> getDomainRegistrationImpl domain

deleteDomainImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
deleteDomainImpl domain = do
  mOld <- tryGetDomainRegistrationImpl domain
  sendAuditMail url "Domain deleted" mOld Nothing
  delete domain
  where
    url :: Builder
    url =
      "DELETE /i/domain-registration/"
        <> fromText (domainText domain)

unauthorizeImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
unauthorizeImpl domain = do
  old <- getDomainRegistrationImpl domain
  let new = old {domainRedirect = None} :: DomainRegistration
  case old.domainRedirect of
    PreAuthorized -> audit old new *> upsert (toStored new)
    Backend _ -> audit old new *> upsert (toStored new)
    NoRegistration -> audit old new *> upsert (toStored new)
    None -> pure ()
    Locked -> throw EnterpriseLoginSubsystemErrorInvalidDomainRedirect
    SSO _ -> throw EnterpriseLoginSubsystemErrorInvalidDomainRedirect
  where
    audit :: DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain unauthorized" (Just old) (Just new)

    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/unauthorized"

updateDomainRegistrationImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Domain ->
  DomainRegistrationUpdate ->
  Sem r ()
updateDomainRegistrationImpl domain update = do
  validate update
  mOld <- tryGetDomainRegistrationImpl domain
  case mOld of
    Just dr -> do
      let new = dr {teamInvite = update.teamInvite, domainRedirect = update.domainRedirect} :: DomainRegistration
      audit mOld new *> upsert (toStored new)
    Nothing -> do
      let new = DomainRegistration domain update.domainRedirect update.teamInvite Nothing
      audit mOld new *> upsert (toStored new)
  where
    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain registration updated" old (Just new)

    url :: Builder
    url =
      "PUT /i/domain-registration/"
        <> fromText (domainText domain)

lockDomainImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
lockDomainImpl domain = do
  mOld <- tryGetDomainRegistrationImpl domain
  let new = DomainRegistration domain Locked Allowed Nothing
  audit mOld new *> upsert (toStored new)
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/lock"

    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" old (Just new)

unlockDomainImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
unlockDomainImpl domain = do
  old <- getDomainRegistrationImpl domain
  let new = old {domainRedirect = None} :: DomainRegistration
  case old.domainRedirect of
    Locked -> audit old new *> upsert (toStored new)
    _ -> throw EnterpriseLoginSubsystemUnlockError
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/unlock"

    audit :: DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" (Just old) (Just new)

preAuthorizeImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r,
    Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member EmailSending r
  ) =>
  Domain ->
  Sem r ()
preAuthorizeImpl domain = do
  mOld <- tryGetDomainRegistrationImpl domain
  case mOld of
    Nothing -> do
      let new = DomainRegistration domain PreAuthorized Allowed Nothing
      audit mOld new *> upsert (toStored new)
    Just sdr | sdr.domainRedirect == None -> do
      let new = sdr {domainRedirect = PreAuthorized} :: DomainRegistration
      audit mOld new *> upsert (toStored new)
    Just sdr | sdr.domainRedirect == PreAuthorized -> pure ()
    _ -> throw EnterpriseLoginSubsystemErrorInvalidDomainRedirect
  where
    url :: Builder
    url =
      "POST /i/domain-registration/"
        <> fromText (domainText domain)
        <> "/preauthorize"

    audit :: Maybe DomainRegistration -> DomainRegistration -> Sem r ()
    audit old new = sendAuditMail url "Domain locked" old (Just new)

getDomainRegistrationImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r DomainRegistration
getDomainRegistrationImpl domain = do
  mSdr <- tryGetDomainRegistrationImpl domain
  case mSdr of
    Just dr -> pure dr
    Nothing -> throw EnterpriseLoginSubsystemErrorNotFound

tryGetDomainRegistrationImpl ::
  forall r.
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r (Maybe DomainRegistration)
tryGetDomainRegistrationImpl domain = do
  mSdr <- lookup domain
  maybe (pure Nothing) (fmap Just . fromStoredWithExcept) mSdr
  where
    fromStoredWithExcept :: StoredDomainRegistration -> Sem r DomainRegistration
    fromStoredWithExcept sdr = do
      case fromStored sdr of
        Nothing -> do
          Log.err $ Log.field "domain" (toByteString' domain) . Log.msg (Log.val "Invalid stored domain registration")
          throw EnterpriseLoginSubsystemInternalError
        Just dr -> pure dr

fromStored :: StoredDomainRegistration -> Maybe DomainRegistration
fromStored sdr =
  DomainRegistration sdr.domain
    <$> getDomainRedirect sdr
    <*> getTeamInvite sdr
    <*> pure sdr.dnsVerificationToken
  where
    getTeamInvite :: StoredDomainRegistration -> Maybe TeamInvite
    getTeamInvite = \case
      StoredDomainRegistration _ _ ti _ _ tid _ -> case (ti, tid) of
        (AllowedTag, Nothing) -> Just Allowed
        (NotAllowedTag, Nothing) -> Just NotAllowed
        (TeamTag, Just teamId) -> Just $ Team teamId
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ ssoId url _ _ -> case (dr, ssoId, url) of
        (NoneTag, Nothing, Nothing) -> Just None
        (LockedTag, Nothing, Nothing) -> Just Locked
        (PreAuthorizedTag, Nothing, Nothing) -> Just PreAuthorized
        (SSOTag, Just idpId, Nothing) -> Just $ SSO idpId
        (BackendTag, Nothing, Just beUrl) -> Just $ Backend beUrl
        (NoRegistrationTag, Nothing, Nothing) -> Just NoRegistration
        _ -> Nothing

toStored :: DomainRegistration -> StoredDomainRegistration
toStored dr =
  let (domainRedirect, idpId, backendUrl) = fromDomainRedirect dr.domainRedirect
      (teamInvite, team) = fromTeamInvite dr.teamInvite
   in StoredDomainRegistration dr.domain domainRedirect teamInvite idpId backendUrl team (dr.dnsVerificationToken)
  where
    fromTeamInvite :: TeamInvite -> (TeamInviteTag, Maybe TeamId)
    fromTeamInvite Allowed = (AllowedTag, Nothing)
    fromTeamInvite NotAllowed = (NotAllowedTag, Nothing)
    fromTeamInvite (Team teamId) = (TeamTag, Just teamId)

    fromDomainRedirect :: DomainRedirect -> (DomainRedirectTag, Maybe SAML.IdPId, Maybe HttpsUrl)
    fromDomainRedirect None = (NoneTag, Nothing, Nothing)
    fromDomainRedirect Locked = (LockedTag, Nothing, Nothing)
    fromDomainRedirect (SSO idpId) = (SSOTag, Just idpId, Nothing)
    fromDomainRedirect (Backend url) = (BackendTag, Nothing, Just url)
    fromDomainRedirect NoRegistration = (NoRegistrationTag, Nothing, Nothing)
    fromDomainRedirect PreAuthorized = (PreAuthorizedTag, Nothing, Nothing)

validate :: (Member (Error EnterpriseLoginSubsystemError) r) => DomainRegistrationUpdate -> Sem r ()
validate dr = do
  case dr.domainRedirect of
    Locked -> when (dr.teamInvite /= Allowed) $ throw (EnterpriseLoginSubsystemErrorUpdateFailure "Team invite must be allowed for a locked domain")
    Backend _ -> when (dr.teamInvite /= NotAllowed) $ throw (EnterpriseLoginSubsystemErrorUpdateFailure "Team invite must not be allowed for a backend domain")
    _ -> pure ()

mkAuditMail :: EmailAddress -> EmailAddress -> Text -> LText -> Mail
mkAuditMail from to subject body =
  (emptyMail (Address Nothing (fromEmail from)))
    { mailTo = [Address Nothing (fromEmail to)],
      mailHeaders =
        [ ("Subject", subject),
          ("X-Zeta-Purpose", "audit")
        ],
      mailParts = [[plainPart body]]
    }

sendAuditMail ::
  ( Member (Input (Maybe EnterpriseLoginSubsystemConfig)) r,
    Member TinyLog r,
    Member EmailSending r
  ) =>
  Builder ->
  Text ->
  Maybe DomainRegistration ->
  Maybe DomainRegistration ->
  Sem r ()
sendAuditMail url subject mBefore mAfter = do
  let auditLog :: LText =
        toLazyText $
          url
            <> " called;\nOld value:\n"
            <> fromLazyText (decodeUtf8 (maybe "null" Aeson.encodePretty mBefore))
            <> "\nNew value:\n"
            <> fromLazyText (decodeUtf8 (maybe "null" Aeson.encodePretty mAfter))
  Log.info $
    Log.msg (Log.val "Domain registration audit log")
      . Log.field "url" (encodeUtf8 $ toLazyText url)
      . Log.field "old_value" (maybe "null" Aeson.encode mBefore)
      . Log.field "new_value" (maybe "null" Aeson.encode mAfter)
  mConfig <- input
  for_ mConfig $ \config -> do
    let mail = mkAuditMail (config.auditEmailSender) (config.auditEmailRecipient) subject auditLog
    sendMail mail
