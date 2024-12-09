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
    LockDomain domain -> upsert $ StoredDomainRegistration domain LockedTag AllowedTag Nothing Nothing Nothing Nothing
    UnlockDomain domain -> unlockDomainImpl domain
    GetDomainRegistration domain -> getDomainRegistrationImpl domain

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

getDomainRegistrationImpl ::
  ( Member DomainRegistrationStore r,
    Member (Error EnterpriseLoginSubsystemError) r,
    Member TinyLog r
  ) =>
  Domain ->
  Sem r DomainRegistration
getDomainRegistrationImpl domain = do
  mStoredDomainRegistration <- lookup domain
  case mStoredDomainRegistration of
    Nothing -> throw EnterpriseLoginSubsystemErrorNotFound
    Just sdr -> do
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
      StoredDomainRegistration _ _ ti _ _ _ _ -> case ti of
        AllowedTag -> Just Allowed
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ _ _ _ _ -> case dr of
        NoneTag -> Just None
        LockedTag -> Just Locked
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
