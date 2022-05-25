-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.Sem.UserQuery
  ( UserQuery (..),
    getId,
    getUsers,
    getName,
    getLocale,
    getAuthentication,
    getPassword,
    getActivated,
    getAccountStatus,
    getAccountStatuses,
    getTeam,
    getAccounts,
    insertAccount,
    updateUser,

    -- * effect-derived functions
    lookupAccount,
    lookupAccounts,

    -- * error types
    AuthError (..),
    ReAuthError (..),

    -- * misc types
    AccountRow,
    UserRow,
    UserRowInsert,
  )
where

import Brig.Password
import Brig.Types
import Brig.Types.Intra
import Data.Domain
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Qualified
import Imports
import Network.HTTP.Types.Status
import qualified Network.Wai.Utilities.Error as Wai
import Polysemy
import Polysemy.Input
import Wire.API.Error
import qualified Wire.API.Error.Brig as E
import Wire.API.Provider.Service

type Activated = Bool

type UserRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

-- Represents a 'UserAccount'
type AccountRow =
  ( UserId,
    Name,
    Maybe Pict,
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe [Asset],
    Activated,
    Maybe AccountStatus,
    Maybe UTCTimeMillis,
    Maybe Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    Maybe ManagedBy
  )

type UserRowInsert =
  ( UserId,
    Name,
    Pict,
    [Asset],
    Maybe Email,
    Maybe Phone,
    Maybe UserSSOId,
    ColourId,
    Maybe Password,
    Activated,
    AccountStatus,
    Maybe UTCTimeMillis,
    Language,
    Maybe Country,
    Maybe ProviderId,
    Maybe ServiceId,
    Maybe Handle,
    Maybe TeamId,
    ManagedBy
  )

deriving instance Show UserRowInsert

-- | Authentication errors.
data AuthError
  = AuthInvalidUser
  | AuthInvalidCredentials
  | AuthSuspended
  | AuthEphemeral
  | AuthPendingInvitation

instance APIError AuthError where
  toWai AuthInvalidUser = errorToWai @'E.BadCredentials
  toWai AuthInvalidCredentials = errorToWai @'E.BadCredentials
  toWai AuthSuspended = accountSuspended
  toWai AuthEphemeral = accountEphemeral
  toWai AuthPendingInvitation = accountPending

-- TODO(md): all the Wai.Error values in this module have been copied from
-- Brig.API.Error to avoid a cycle in module imports. Fix that.
accountSuspended :: Wai.Error
accountSuspended = Wai.mkError status403 "suspended" "Account suspended."

accountEphemeral :: Wai.Error
accountEphemeral = Wai.mkError status403 "ephemeral" "Account is ephemeral."

accountPending :: Wai.Error
accountPending = Wai.mkError status403 "pending-activation" "Account pending activation."

-- | Re-authentication errors.
data ReAuthError
  = ReAuthError !AuthError
  | ReAuthMissingPassword
  | ReAuthCodeVerificationRequired
  | ReAuthCodeVerificationNoPendingCode
  | ReAuthCodeVerificationNoEmail

instance APIError ReAuthError where
  toWai (ReAuthError e) = toWai e
  toWai ReAuthMissingPassword = errorToWai @'E.MissingAuth
  toWai ReAuthCodeVerificationRequired = verificationCodeRequired
  toWai ReAuthCodeVerificationNoPendingCode = verificationCodeNoPendingCode
  toWai ReAuthCodeVerificationNoEmail = verificationCodeNoEmail

verificationCodeRequired :: Wai.Error
verificationCodeRequired = Wai.mkError status403 "code-authentication-required" "Verification code required."

verificationCodeNoPendingCode :: Wai.Error
verificationCodeNoPendingCode = Wai.mkError status403 "code-authentication-failed" "Code authentication failed (no such code)."

verificationCodeNoEmail :: Wai.Error
verificationCodeNoEmail = Wai.mkError status403 "code-authentication-failed" "Code authentication failed (no such email)."

-------------------------------------------------------------------------------
-- Conversions

-- | Construct a 'UserAccount' from a raw user record in the database.
toUserAccount :: Domain -> Locale -> AccountRow -> UserAccount
toUserAccount
  domain
  defaultLocale
  ( uid,
    name,
    pict,
    email,
    phone,
    ssoid,
    accent,
    assets,
    activated,
    status,
    expires,
    lan,
    con,
    pid,
    sid,
    handle,
    tid,
    managed_by
    ) =
    let ident = toIdentity activated email phone ssoid
        deleted = Just Deleted == status
        expiration = if status == Just Ephemeral then expires else Nothing
        loc = toLocale defaultLocale (lan, con)
        svc = newServiceRef <$> sid <*> pid
     in UserAccount
          ( User
              uid
              (Qualified uid domain)
              ident
              name
              (fromMaybe noPict pict)
              (fromMaybe [] assets)
              accent
              deleted
              loc
              svc
              handle
              expiration
              tid
              (fromMaybe ManagedByWire managed_by)
          )
          (fromMaybe Active status)

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _ = l

-- | Construct a 'UserIdentity'.
--
-- If the user is not activated, 'toIdentity' will return 'Nothing' as a precaution, because
-- elsewhere we rely on the fact that a non-empty 'UserIdentity' means that the user is
-- activated.
--
-- The reason it's just a "precaution" is that we /also/ have an invariant that having an
-- email or phone in the database means the user has to be activated.
toIdentity ::
  -- | Whether the user is activated
  Bool ->
  Maybe Email ->
  Maybe Phone ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) (Just p) Nothing = Just $! FullIdentity e p
toIdentity True (Just e) Nothing Nothing = Just $! EmailIdentity e
toIdentity True Nothing (Just p) Nothing = Just $! PhoneIdentity p
toIdentity True email phone (Just ssoid) = Just $! SSOIdentity ssoid email phone
toIdentity True Nothing Nothing Nothing = Nothing
toIdentity False _ _ _ = Nothing

-------------------------------------------------------------------------------

data UserQuery m a where
  GetId :: UserId -> UserQuery m (Maybe UserId) -- idSelect
  GetUsers :: [UserId] -> UserQuery m [UserRow] -- usersSelect
  GetName :: UserId -> UserQuery m (Maybe Name) -- nameSelect
  GetLocale :: UserId -> UserQuery m (Maybe (Maybe Language, Maybe Country)) -- localeSelect
  GetAuthentication :: UserId -> UserQuery m (Maybe (Maybe Password, Maybe AccountStatus)) -- authSelect
  GetPassword :: UserId -> UserQuery m (Maybe Password) -- passwordSelect
  GetActivated :: UserId -> UserQuery m Bool -- activatedSelect
  GetAccountStatus :: UserId -> UserQuery m (Maybe AccountStatus) -- statusSelect
  GetAccountStatuses :: [UserId] -> UserQuery m [(UserId, Bool, Maybe AccountStatus)] -- accountStateSelectAll
  GetTeam :: UserId -> UserQuery m (Maybe TeamId) -- teamSelect
  GetAccounts :: [UserId] -> UserQuery m [AccountRow] -- accountsSelect
  -- FUTUREWORK: The 'InsertAccount' action should perhaps be in an account store effect
  InsertAccount ::
    UserAccount ->
    -- | If a bot: conversation and team
    --   (if a team conversation)
    Maybe (ConvId, Maybe TeamId) ->
    Maybe Password ->
    -- | Whether the user is activated
    Bool ->
    UserQuery m ()
  UpdateUser :: UserId -> UserUpdate -> UserQuery m ()

makeSem ''UserQuery

lookupAccount ::
  Members '[Input (Local ()), UserQuery] r =>
  Locale ->
  UserId ->
  Sem r (Maybe UserAccount)
lookupAccount locale u = listToMaybe <$> lookupAccounts locale [u]

lookupAccounts ::
  Members '[Input (Local ()), UserQuery] r =>
  Locale ->
  [UserId] ->
  Sem r [UserAccount]
lookupAccounts locale users = do
  domain <- tDomain <$> input @(Local ())
  fmap (toUserAccount domain locale) <$> getAccounts users
