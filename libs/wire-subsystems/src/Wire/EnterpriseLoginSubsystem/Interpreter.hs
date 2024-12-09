{-# LANGUAGE OverloadedRecordDot #-}

module Wire.EnterpriseLoginSubsystem.Interpreter
  ( runEnterpriseLoginSubsystem,
  )
where

import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Imports hiding (lookup)
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
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
    GetDomainRegistration domain -> getDomainRegistrationImpl domain

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
