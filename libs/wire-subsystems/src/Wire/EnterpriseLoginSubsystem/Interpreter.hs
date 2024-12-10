{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.EnterpriseLoginSubsystem.Interpreter
  ( runEnterpriseLoginSubsystem,
  )
where

import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Id
import Data.Misc (HttpsUrl (..))
import Imports hiding (lookup)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Wire.API.EnterpriseLogin
import Wire.DomainRegistrationStore
import Wire.EnterpriseLoginSubsystem
import Wire.EnterpriseLoginSubsystem.Error

runEnterpriseLoginSubsystem ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
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

deleteDomainImpl :: (Member DomainRegistrationStore r) => Domain -> Sem r ()
deleteDomainImpl domain = delete domain

unauthorizeImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r ()
unauthorizeImpl domain = do
  dr <- getDomainRegistrationImpl domain
  case dr.domainRedirect of
    PreAuthorized -> upsert $ toStored dr {domainRedirect = None}
    Backend _ -> upsert $ toStored dr {domainRedirect = None}
    NoRegistration -> upsert $ toStored dr {domainRedirect = None}
    None -> pure ()
    Locked -> throw EnterpriseLoginSubsystemErrorInvalidDomainRedirect
    SSO _ -> throw EnterpriseLoginSubsystemErrorInvalidDomainRedirect

updateDomainRegistrationImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  DomainRegistrationUpdate ->
  Sem r ()
updateDomainRegistrationImpl domain update = do
  validate update
  mDr <- tryGetDomainRegistrationImpl domain
  case mDr of
    Just dr -> do
      let dr' = dr {teamInvite = update.teamInvite, domainRedirect = update.domainRedirect} :: DomainRegistration
      upsert $ toStored dr'
    Nothing -> upsert $ toStored $ DomainRegistration domain update.domainRedirect update.teamInvite Nothing

lockDomainImpl ::
  ( Member DomainRegistrationStore r
  ) =>
  Domain ->
  Sem r ()
lockDomainImpl domain =
  upsert $ StoredDomainRegistration domain LockedTag AllowedTag Nothing Nothing Nothing Nothing

unlockDomainImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r ()
unlockDomainImpl domain = do
  dr <- getDomainRegistrationImpl domain
  case dr.domainRedirect of
    Locked -> upsert $ toStored $ dr {domainRedirect = None}
    _ -> throw EnterpriseLoginSubsystemUnlockError

preAuthorizeImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r
  ) =>
  Domain ->
  Sem r ()
preAuthorizeImpl domain = do
  mDr <- lookup domain
  case mDr of
    Nothing -> upsert $ StoredDomainRegistration domain PreAuthorizedTag AllowedTag Nothing Nothing Nothing Nothing
    Just sdr | sdr.domainRedirect == NoneTag -> upsert $ sdr {domainRedirect = PreAuthorizedTag}
    Just sdr | sdr.domainRedirect == PreAuthorizedTag -> pure ()
    _ -> throw EnterpriseLoginSubsystemErrorInvalidDomainRedirect

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
  maybe (pure Nothing) (fmap Just . deserialize) mSdr
  where
    deserialize :: StoredDomainRegistration -> Sem r DomainRegistration
    deserialize sdr = do
      let mDomainRegistration = fromStored sdr
      case mDomainRegistration of
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
        (AllowedTag, _) -> Just Allowed
        (NotAllowedTag, _) -> Just NotAllowed
        (TeamTag, Just teamId) -> Just $ Team teamId
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ ssoId url _ _ -> case (dr, ssoId, url) of
        (NoneTag, _, _) -> Just None
        (LockedTag, _, _) -> Just Locked
        (PreAuthorizedTag, _, _) -> Just PreAuthorized
        (SSOTag, Just idpId, _) -> Just $ SSO idpId
        (BackendTag, _, Just beUrl) -> Just $ Backend beUrl
        (NoRegistrationTag, _, _) -> Just NoRegistration
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
    Backend _ -> when (dr.teamInvite /= NotAllowed) $ throw (EnterpriseLoginSubsystemErrorUpdateFailure "Team invite must be not-allowed for a backend domain")
    _ -> pure ()
