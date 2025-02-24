{-# LANGUAGE TemplateHaskell #-}

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

module Spar.Sem.BrigAccess
  ( BrigAccess (..),
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
    deleteUser,
    ensureReAuthorised,
    ssoLogin,
    getStatus,
    getStatusMaybe,
    setStatus,
    getDefaultUserLocale,
  )
where

import Brig.Types.Intra
import Data.Code as Code
import Data.Handle (Handle)
import Data.HavePendingInvitations
import Data.Id (TeamId, UserId)
import Data.Misc (PlainTextPassword6)
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import Web.Cookie
import Wire.API.Locale
import Wire.API.Team.Role
import Wire.API.User
import Wire.API.User.RichInfo as RichInfo

data BrigAccess m a where
  CreateSAML :: SAML.UserRef -> UserId -> TeamId -> Name -> ManagedBy -> Maybe Handle -> Maybe RichInfo -> Maybe Locale -> Role -> BrigAccess m UserId
  CreateNoSAML :: Text -> EmailAddress -> UserId -> TeamId -> Name -> Maybe Locale -> Role -> BrigAccess m UserId
  UpdateEmail :: UserId -> EmailAddress -> BrigAccess m ()
  GetAccount :: HavePendingInvitations -> UserId -> BrigAccess m (Maybe User)
  GetByHandle :: Handle -> BrigAccess m (Maybe User)
  GetByEmail :: EmailAddress -> BrigAccess m (Maybe User)
  SetName :: UserId -> Name -> BrigAccess m ()
  SetHandle :: UserId -> Handle {- not 'HandleUpdate'! -} -> BrigAccess m ()
  SetManagedBy :: UserId -> ManagedBy -> BrigAccess m ()
  SetSSOId :: UserId -> UserSSOId -> BrigAccess m ()
  SetRichInfo :: UserId -> RichInfo -> BrigAccess m ()
  SetLocale :: UserId -> Maybe Locale -> BrigAccess m ()
  GetRichInfo :: UserId -> BrigAccess m RichInfo
  CheckHandleAvailable :: Handle -> BrigAccess m Bool
  DeleteUser :: UserId -> BrigAccess m DeleteUserResult
  EnsureReAuthorised :: Maybe UserId -> Maybe PlainTextPassword6 -> Maybe Code.Value -> Maybe VerificationAction -> BrigAccess m ()
  SsoLogin :: UserId -> BrigAccess m SetCookie
  GetStatus :: UserId -> BrigAccess m AccountStatus
  GetStatusMaybe :: UserId -> BrigAccess m (Maybe AccountStatus)
  SetStatus :: UserId -> AccountStatus -> BrigAccess m ()
  GetDefaultUserLocale :: BrigAccess m Locale
  GetDomainRegistration :: EmailAddress -> BrigAccess m DomainRedirectResponse

makeSem ''BrigAccess
