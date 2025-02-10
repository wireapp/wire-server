{-# LANGUAGE TemplateHaskell #-}

module Wire.DomainRegistrationStore
  ( DomainRegistrationStore (..),
    StoredDomainRegistration (..),
    DomainKey,
    upsert,
    lookup,
    lookupByTeam,
    delete,
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.CaseInsensitive
import Data.CaseInsensitive qualified as CI
import Data.Domain as Domain
import Data.Id
import Data.Misc
import Data.Text as T
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports hiding (lookup)
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Wire.API.EnterpriseLogin

newtype DomainKey = DomainKey {unDomainKey :: CI Text}
  deriving stock (Eq, Ord, Show)

mkDomainKey :: Domain -> DomainKey
mkDomainKey = DomainKey . CI.mk . domainText

unmkDomainKey :: DomainKey -> Domain
unmkDomainKey = Domain . CI.foldedCase . unDomainKey

instance Cql DomainKey where
  ctype = Tagged TextColumn
  toCql = CqlText . CI.foldedCase . unDomainKey
  fromCql (CqlText txt) = pure . DomainKey . CI.mk $ txt
  fromCql _ = Left "DomainKey: Text expected"

data StoredDomainRegistration = StoredDomainRegistration
  { domain :: DomainKey,
    domainRedirect :: Maybe DomainRedirectTag,
    teamInvite :: Maybe TeamInviteTag,
    idpId :: Maybe SAML.IdPId,
    backendUrl :: Maybe HttpsUrl,
    team :: Maybe TeamId,
    dnsVerificationToken :: Maybe DnsVerificationToken,
    authTokenHash :: Maybe Token,
    authorizedTeam :: Maybe TeamId
  }
  deriving (Show, Eq, Ord, Generic)

recordInstance ''StoredDomainRegistration

data DomainRegistrationStore m a where
  UpsertInternal :: StoredDomainRegistration -> DomainRegistrationStore m ()
  LookupInternal :: DomainKey -> DomainRegistrationStore m (Maybe StoredDomainRegistration)
  LookupByTeamInternal :: TeamId -> DomainRegistrationStore m [StoredDomainRegistration]
  DeleteInternal :: DomainKey -> DomainRegistrationStore m ()

upsert :: (Member DomainRegistrationStore r) => DomainRegistration -> Sem r ()
upsert = send . UpsertInternal . toStored

lookupByTeam :: forall r. (Member DomainRegistrationStore r, Member (Log.Logger (Log.Msg -> Log.Msg)) r) => TeamId -> Sem r [DomainRegistration]
lookupByTeam tid = do
  rows <- send (LookupByTeamInternal tid)
  mRegisteredDomains <- for rows fromStoredWithLogging
  pure $ catMaybes mRegisteredDomains
  where
    fromStoredWithLogging :: StoredDomainRegistration -> Sem r (Maybe DomainRegistration)
    fromStoredWithLogging row = case fromStored row of
      Just dr -> pure (Just dr)
      Nothing -> logInvalidDomainRegistrationError (unmkDomainKey row.domain) $> Nothing

lookup ::
  forall r.
  (Member DomainRegistrationStore r, Member TinyLog r) =>
  Domain ->
  Sem r (Maybe DomainRegistration)
lookup domain =
  (>>= logErrors) . runError @Bool $ do
    sdr <- send (LookupInternal (mkDomainKey domain)) >>= note False
    fromStored sdr & note True
  where
    logErrors :: Either Bool a -> Sem r (Maybe a)
    logErrors (Left False) = pure Nothing
    logErrors (Left True) = logInvalidDomainRegistrationError domain $> Nothing
    logErrors (Right x) = pure (Just x)

logInvalidDomainRegistrationError :: (Member TinyLog r, ToByteString a) => a -> Sem r ()
logInvalidDomainRegistrationError domain =
  Log.err $
    Log.field "domain" (toByteString' domain)
      . Log.msg (Log.val "Invalid stored domain registration")

delete :: (Member DomainRegistrationStore r) => Domain -> Sem r ()
delete = send . DeleteInternal . mkDomainKey

-- | Inconsistently stored domain registrations (according to 'domainRegistrationFromRow') are
-- discarded.
fromStored :: StoredDomainRegistration -> Maybe DomainRegistration
fromStored sdr = do
  row <-
    DomainRegistrationRow (unmkDomainKey sdr.domain) sdr.authorizedTeam
      <$> getDomainRedirect sdr
      <*> getTeamInvite sdr
      <*> pure sdr.dnsVerificationToken
      <*> pure sdr.authTokenHash
  either (const Nothing) Just (domainRegistrationFromRow row)
  where
    getTeamInvite :: StoredDomainRegistration -> Maybe TeamInvite
    getTeamInvite = \case
      StoredDomainRegistration _ _ ti _ _ tid _ _ _ -> case (ti, tid) of
        (Just AllowedTag, Nothing) -> Just Allowed
        (Just NotAllowedTag, Nothing) -> Just NotAllowed
        (Just TeamTag, Just teamId) -> Just $ Team teamId
        (Nothing, Nothing) -> Just Allowed
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ ssoId url _ _ _ _ -> case (dr, ssoId, url) of
        (Just NoneTag, Nothing, Nothing) -> Just None
        (Just LockedTag, Nothing, Nothing) -> Just Locked
        (Just PreAuthorizedTag, Nothing, Nothing) -> Just PreAuthorized
        (Just SSOTag, Just idpId, Nothing) -> Just $ SSO idpId
        (Just BackendTag, Nothing, Just beUrl) -> Just $ Backend beUrl
        (Just NoRegistrationTag, Nothing, Nothing) -> Just NoRegistration
        (Nothing, Nothing, Nothing) -> Just None
        _ -> Nothing

toStored :: DomainRegistration -> StoredDomainRegistration
toStored (domainRegistrationToRow -> dr) =
  let (domainRedirect, idpId, backendUrl) = fromDomainRedirect dr.domainRedirect
      (teamInvite, team) = fromTeamInvite dr.teamInvite
   in StoredDomainRegistration
        { domain = mkDomainKey dr.domain,
          domainRedirect = Just domainRedirect,
          teamInvite = Just teamInvite,
          idpId,
          backendUrl,
          team,
          dnsVerificationToken = dr.dnsVerificationToken,
          authTokenHash = dr.authTokenHash,
          authorizedTeam = dr.authorizedTeam
        }
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
