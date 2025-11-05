{-# LANGUAGE TemplateHaskell #-}

module Wire.BrigAPIAccess
  ( -- * Brig access effect
    BrigAPIAccess (..),

    -- * Connections
    getConnectionsUnqualified,
    getConnectionsUnqualifiedBidi,
    getConnections,
    putConnectionInternal,

    -- * Users
    reauthUser,
    lookupActivatedUsers,
    getUser,
    getUsers,
    deleteUser,
    getContactList,
    getRichInfoMultiUser,
    getUserExportData,
    updateSearchIndex,
    getAccountsBy,
    createSAML,
    createNoSAML,
    updateEmail,
    getAccount,
    getByHandle,
    getByEmail,
    setName,
    setHandle,
    setManagedBy,
    setSSOId,
    setRichInfo,
    setLocale,
    getRichInfo,
    checkHandleAvailable,
    ensureReAuthorised,
    ssoLogin,
    getStatus,
    getStatusMaybe,
    setStatus,
    getDefaultUserLocale,
    checkAdminGetTeamId,

    -- * Teams
    getSize,

    -- * Clients
    lookupClients,
    lookupClientsFull,
    notifyClientsAboutLegalHoldRequest,
    getLegalHoldAuthToken,
    addLegalHoldClientToUser,
    removeLegalHoldClientFromUser,
    OpaqueAuthToken (..),

    -- * MLS
    getLocalMLSClients,
    getLocalMLSClient,

    -- * Features
    getAccountConferenceCallingConfigClient,
    updateSearchVisibilityInbound,

    -- * Bots
    deleteBot,

    -- * User Groups
    createGroupFull,
  )
where

import Data.Aeson
import Data.ByteString.Conversion
import Data.Code as Code
import Data.Handle (Handle)
import Data.HavePendingInvitations
import Data.Id
import Data.Misc
import Data.Qualified
import Imports
import Network.HTTP.Types.Status
import Polysemy
import Polysemy.Error
import SAML2.WebSSO qualified as SAML
import Web.Cookie
import Wire.API.Connection
import Wire.API.Error.Galley
import Wire.API.Locale
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.Internal.Brig (GetBy)
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Export
import Wire.API.Team.Feature
import Wire.API.Team.Role
import Wire.API.Team.Size
import Wire.API.User
import Wire.API.User.Auth.ReAuth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.RichInfo
import Wire.API.UserGroup (NewUserGroup, UserGroup)

-- | When receiving tokens from other services which are 'just passing through'
-- it's error-prone useless extra work to parse and render them from JSON over and over again.
-- We'll just wrap them with this to give some level of typesafety and a reasonable JSON
-- instance
newtype OpaqueAuthToken = OpaqueAuthToken
  { opaqueAuthTokenToText :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON, ToByteString)

data BrigAPIAccess m a where
  GetConnectionsUnqualified ::
    [UserId] ->
    Maybe [UserId] ->
    Maybe Relation ->
    BrigAPIAccess m [ConnectionStatus]
  GetConnections ::
    [UserId] ->
    Maybe [Qualified UserId] ->
    Maybe Relation ->
    BrigAPIAccess m [ConnectionStatusV2]
  PutConnectionInternal :: UpdateConnectionsInternal -> BrigAPIAccess m Status
  ReauthUser :: UserId -> ReAuthUser -> BrigAPIAccess m (Either AuthenticationError ())
  LookupActivatedUsers :: [UserId] -> BrigAPIAccess m [User]
  GetUsers :: [UserId] -> BrigAPIAccess m [User]
  DeleteUser :: UserId -> BrigAPIAccess m ()
  GetContactList :: UserId -> BrigAPIAccess m [UserId]
  GetRichInfoMultiUser :: [UserId] -> BrigAPIAccess m [(UserId, RichInfo)]
  GetSize :: TeamId -> BrigAPIAccess m TeamSize
  LookupClients :: [UserId] -> BrigAPIAccess m UserClients
  LookupClientsFull :: [UserId] -> BrigAPIAccess m UserClientsFull
  NotifyClientsAboutLegalHoldRequest ::
    UserId ->
    UserId ->
    LastPrekey ->
    BrigAPIAccess m ()
  GetLegalHoldAuthToken ::
    UserId ->
    Maybe PlainTextPassword6 ->
    BrigAPIAccess m OpaqueAuthToken
  AddLegalHoldClientToUserEither ::
    UserId ->
    ConnId ->
    [Prekey] ->
    LastPrekey ->
    BrigAPIAccess m (Either AuthenticationError ClientId)
  RemoveLegalHoldClientFromUser :: UserId -> BrigAPIAccess m ()
  GetAccountConferenceCallingConfigClient :: UserId -> BrigAPIAccess m (Feature ConferenceCallingConfig)
  GetLocalMLSClients :: Local UserId -> CipherSuiteTag -> BrigAPIAccess m (Set ClientInfo)
  GetLocalMLSClient :: Local UserId -> ClientId -> CipherSuiteTag -> BrigAPIAccess m ClientInfo
  UpdateSearchVisibilityInbound ::
    Multi.TeamStatus SearchVisibilityInboundConfig ->
    BrigAPIAccess m ()
  GetUserExportData :: UserId -> BrigAPIAccess m (Maybe TeamExportUser)
  DeleteBot :: ConvId -> BotId -> BrigAPIAccess m ()
  UpdateSearchIndex :: UserId -> BrigAPIAccess m ()
  GetAccountsBy :: GetBy -> BrigAPIAccess m [User]
  CreateGroupFull :: ManagedBy -> TeamId -> Maybe UserId -> NewUserGroup -> BrigAPIAccess m UserGroup
  CreateSAML :: SAML.UserRef -> UserId -> TeamId -> Name -> ManagedBy -> Maybe Handle -> Maybe RichInfo -> Maybe Locale -> Role -> BrigAPIAccess m UserId
  CreateNoSAML :: Text -> EmailAddress -> UserId -> TeamId -> Name -> Maybe Locale -> Role -> BrigAPIAccess m UserId
  UpdateEmail :: UserId -> EmailAddress -> EmailActivation -> BrigAPIAccess m ()
  GetAccount :: HavePendingInvitations -> UserId -> BrigAPIAccess m (Maybe User)
  GetByHandle :: Handle -> BrigAPIAccess m (Maybe User)
  GetByEmail :: EmailAddress -> BrigAPIAccess m (Maybe User)
  SetName :: UserId -> Name -> BrigAPIAccess m ()
  SetHandle :: UserId -> Handle -> BrigAPIAccess m ()
  SetManagedBy :: UserId -> ManagedBy -> BrigAPIAccess m ()
  SetSSOId :: UserId -> UserSSOId -> BrigAPIAccess m ()
  SetRichInfo :: UserId -> RichInfo -> BrigAPIAccess m ()
  SetLocale :: UserId -> Maybe Locale -> BrigAPIAccess m ()
  GetRichInfo :: UserId -> BrigAPIAccess m RichInfo
  CheckHandleAvailable :: Handle -> BrigAPIAccess m Bool
  EnsureReAuthorised :: Maybe UserId -> Maybe PlainTextPassword6 -> Maybe Code.Value -> Maybe VerificationAction -> BrigAPIAccess m ()
  SsoLogin :: UserId -> BrigAPIAccess m SetCookie
  GetStatus :: UserId -> BrigAPIAccess m AccountStatus
  GetStatusMaybe :: UserId -> BrigAPIAccess m (Maybe AccountStatus)
  SetStatus :: UserId -> AccountStatus -> BrigAPIAccess m ()
  GetDefaultUserLocale :: BrigAPIAccess m Locale
  CheckAdminGetTeamId :: UserId -> BrigAPIAccess m TeamId

makeSem ''BrigAPIAccess

getUser :: (Member BrigAPIAccess r) => UserId -> Sem r (Maybe User)
getUser = fmap listToMaybe . getUsers . pure

addLegalHoldClientToUser ::
  (Member BrigAPIAccess r, Member (Error AuthenticationError) r) =>
  UserId ->
  ConnId ->
  [Prekey] ->
  LastPrekey ->
  Sem r ClientId
addLegalHoldClientToUser uid con pks lpk =
  addLegalHoldClientToUserEither uid con pks lpk
    >>= either throw pure

getConnectionsUnqualifiedBidi :: (Member BrigAPIAccess r) => [UserId] -> [UserId] -> Maybe Relation -> Maybe Relation -> Sem r ([ConnectionStatus], [ConnectionStatus])
getConnectionsUnqualifiedBidi uids1 uids2 mrel1 mrel2 = do
  res1 <- getConnectionsUnqualified uids1 (Just uids2) mrel1
  res2 <- getConnectionsUnqualified uids2 (Just uids1) mrel2
  pure (res1, res2)
