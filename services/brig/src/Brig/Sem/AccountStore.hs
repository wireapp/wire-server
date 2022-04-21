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

module Brig.Sem.AccountStore where

import Brig.Password
import Brig.Types.Intra
import Data.Handle
import Data.Id
import Data.Misc
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.User

data AccountStore m a where
  NewAccount ::
    NewUser ->
    Maybe InvitationId ->
    Maybe TeamId ->
    Maybe Handle ->
    AccountStore m (UserAccount, Maybe Password)
  InsertAccount ::
    UserAccount ->
    -- | If a bot: conversation and team
    --   (if a team conversation)
    Maybe (ConvId, Maybe TeamId) ->
    Maybe Password ->
    -- | Whether the user is activated
    Bool ->
    AccountStore m ()
  LookupAuth :: UserId -> AccountStore m (Maybe (Maybe Password, AccountStatus))

makeSem ''AccountStore

-- | Authentication errors.
data AuthError
  = AuthInvalidUser
  | AuthInvalidCredentials
  | AuthSuspended
  | AuthEphemeral
  | AuthPendingInvitation

-- | Re-authentication errors.
data ReAuthError
  = ReAuthError !AuthError
  | ReAuthMissingPassword
  | ReAuthCodeVerificationRequired
  | ReAuthCodeVerificationNoPendingCode
  | ReAuthCodeVerificationNoEmail

-- | Mandatory password authentication.
authenticate ::
  Members '[AccountStore, Error AuthError] r =>
  UserId ->
  PlainTextPassword ->
  Sem r ()
authenticate u pw =
  lookupAuth u >>= \case
    Nothing -> throw AuthInvalidUser
    Just (_, Deleted) -> throw AuthInvalidUser
    Just (_, Suspended) -> throw AuthSuspended
    Just (_, Ephemeral) -> throw AuthEphemeral
    Just (_, PendingInvitation) -> throw AuthPendingInvitation
    Just (Nothing, _) -> throw AuthInvalidCredentials
    Just (Just pw', Active) ->
      unless (verifyPassword pw pw') $
        throw AuthInvalidCredentials

-- | Password reauthentication. If the account has a password, reauthentication
-- is mandatory. If the account has no password and no password is given,
-- reauthentication is a no-op.
reauthenticate ::
  Members '[AccountStore, Error ReAuthError] r =>
  UserId ->
  Maybe PlainTextPassword ->
  Sem r ()
reauthenticate u pw =
  lookupAuth u >>= \case
    Nothing -> throw (ReAuthError AuthInvalidUser)
    Just (_, Deleted) -> throw (ReAuthError AuthInvalidUser)
    Just (_, Suspended) -> throw (ReAuthError AuthSuspended)
    Just (_, PendingInvitation) -> throw (ReAuthError AuthPendingInvitation)
    Just (Nothing, _) -> for_ pw $ const (throw $ ReAuthError AuthInvalidCredentials)
    Just (Just pw', Active) -> maybeReAuth pw'
    Just (Just pw', Ephemeral) -> maybeReAuth pw'
  where
    maybeReAuth pw' = case pw of
      Nothing -> throw ReAuthMissingPassword
      Just p ->
        unless (verifyPassword p pw') $
          throw (ReAuthError AuthInvalidCredentials)
