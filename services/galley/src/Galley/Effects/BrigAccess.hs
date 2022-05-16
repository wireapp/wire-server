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

module Galley.Effects.BrigAccess
  ( -- * Brig access effect
    BrigAccess (..),

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

    -- * Teams
    getSize,

    -- * Clients
    lookupClients,
    lookupClientsFull,
    notifyClientsAboutLegalHoldRequest,
    getLegalHoldAuthToken,
    addLegalHoldClientToUser,
    removeLegalHoldClientFromUser,

    -- * MLS
    getClientByKeyPackageRef,
    getLocalMLSClients,

    -- * Features
    getAccountFeatureConfigClient,
    updateSearchVisibilityInbound,
  )
where

import Brig.Types.Client
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.User
import Data.Id
import Data.Misc
import Data.Qualified
import Galley.External.LegalHoldService.Types
import Imports
import Network.HTTP.Types.Status
import Polysemy
import Polysemy.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.Routes.Internal.Brig.Connection
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Team.Feature
import Wire.API.Team.Size
import Wire.API.User.Client
import Wire.API.User.RichInfo

data BrigAccess m a where
  GetConnectionsUnqualified ::
    [UserId] ->
    Maybe [UserId] ->
    Maybe Relation ->
    BrigAccess m [ConnectionStatus]
  GetConnectionsUnqualifiedBidi ::
    [UserId] ->
    [UserId] ->
    Maybe Relation ->
    Maybe Relation ->
    BrigAccess m ([ConnectionStatus], [ConnectionStatus])
  GetConnections ::
    [UserId] ->
    Maybe [Qualified UserId] ->
    Maybe Relation ->
    BrigAccess m [ConnectionStatusV2]
  PutConnectionInternal :: UpdateConnectionsInternal -> BrigAccess m Status
  ReauthUser :: UserId -> ReAuthUser -> BrigAccess m (Either AuthenticationError ())
  LookupActivatedUsers :: [UserId] -> BrigAccess m [User]
  GetUsers :: [UserId] -> BrigAccess m [UserAccount]
  DeleteUser :: UserId -> BrigAccess m ()
  GetContactList :: UserId -> BrigAccess m [UserId]
  GetRichInfoMultiUser :: [UserId] -> BrigAccess m [(UserId, RichInfo)]
  GetSize :: TeamId -> BrigAccess m TeamSize
  LookupClients :: [UserId] -> BrigAccess m UserClients
  LookupClientsFull :: [UserId] -> BrigAccess m UserClientsFull
  NotifyClientsAboutLegalHoldRequest ::
    UserId ->
    UserId ->
    LastPrekey ->
    BrigAccess m ()
  GetLegalHoldAuthToken ::
    UserId ->
    Maybe PlainTextPassword ->
    BrigAccess m OpaqueAuthToken
  AddLegalHoldClientToUserEither ::
    UserId ->
    ConnId ->
    [Prekey] ->
    LastPrekey ->
    BrigAccess m (Either AuthenticationError ClientId)
  RemoveLegalHoldClientFromUser :: UserId -> BrigAccess m ()
  GetAccountFeatureConfigClient :: UserId -> BrigAccess m TeamFeatureStatusNoConfig
  GetClientByKeyPackageRef :: KeyPackageRef -> BrigAccess m (Maybe ClientIdentity)
  GetLocalMLSClients :: Local UserId -> SignatureSchemeTag -> BrigAccess m (Set ClientId)
  UpdateSearchVisibilityInbound ::
    Multi.TeamStatusUpdate 'TeamFeatureSearchVisibilityInbound ->
    BrigAccess m ()

makeSem ''BrigAccess

getUser :: Member BrigAccess r => UserId -> Sem r (Maybe UserAccount)
getUser = fmap listToMaybe . getUsers . pure

addLegalHoldClientToUser ::
  (Member BrigAccess r, Member (Error AuthenticationError) r) =>
  UserId ->
  ConnId ->
  [Prekey] ->
  LastPrekey ->
  Sem r ClientId
addLegalHoldClientToUser uid con pks lpk =
  addLegalHoldClientToUserEither uid con pks lpk
    >>= either throw pure
