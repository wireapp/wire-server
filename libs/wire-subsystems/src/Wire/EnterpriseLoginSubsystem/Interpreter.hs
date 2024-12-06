{-# LANGUAGE OverloadedRecordDot #-}

module Wire.EnterpriseLoginSubsystem.Interpreter
  ( runEnterpriseLoginSubsystem,
  )
where

import Imports hiding (lookup)
import Polysemy
import Wire.API.EnterpriseLogin
import Wire.DomainRegistrationStore
import Wire.EnterpriseLoginSubsystem

runEnterpriseLoginSubsystem ::
  ( Member DomainRegistrationStore r
  ) =>
  Sem (EnterpriseLoginSubsystem ': r) a ->
  Sem r a
runEnterpriseLoginSubsystem = interpret $
  \case
    LockDomain domain -> upsert $ StoredDomainRegistration domain LockedTag AllowedTag Nothing Nothing Nothing Nothing
    GetDomainRegistration domain -> do
      mStoredDomainRegistration <- lookup domain
      let mDomainRegistration = mStoredDomainRegistration >>= fromStored
      pure mDomainRegistration

fromStored :: StoredDomainRegistration -> Maybe DomainRegistration
fromStored sdr =
  DomainRegistration sdr.domain
    <$> getDomainRedirect sdr
    <*> getTeamInvite sdr
    <*> pure sdr.dnsVerificationToken

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
