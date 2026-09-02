-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.BrigAPIAccess where

import Imports
import Polysemy
import Wire.API.Team.Size
import Wire.BrigAPIAccess

-- | Errors out on everything except 'UpdateSearchIndex', which is a no-op.
mockBrigAPIAccess :: InterpreterFor BrigAPIAccess r
mockBrigAPIAccess = interpret $ \case
  UpdateSearchIndex _ -> pure ()
  GetSize {} -> pure (TeamSize 1 0 0)
  -- everything else is not implemented
  GetConnectionsUnqualified {} -> error "GetConnectionsUnqualified: implement on demand (mockBrigAPIAccess)"
  GetConnections {} -> error "GetConnections: implement on demand (mockBrigAPIAccess)"
  PutConnectionInternal {} -> error "PutConnectionInternal: implement on demand (mockBrigAPIAccess)"
  ReauthUser {} -> error "ReauthUser: implement on demand (mockBrigAPIAccess)"
  LookupActivatedUsers {} -> error "LookupActivatedUsers: implement on demand (mockBrigAPIAccess)"
  GetUsers {} -> error "GetUsers: implement on demand (mockBrigAPIAccess)"
  DeleteUser {} -> error "DeleteUser: implement on demand (mockBrigAPIAccess)"
  GetContactList {} -> error "GetContactList: implement on demand (mockBrigAPIAccess)"
  LookupClients {} -> error "LookupClients: implement on demand (mockBrigAPIAccess)"
  LookupClientsFull {} -> error "LookupClientsFull: implement on demand (mockBrigAPIAccess)"
  NotifyClientsAboutLegalHoldRequest {} -> error "NotifyClientsAboutLegalHoldRequest: implement on demand (mockBrigAPIAccess)"
  GetLegalHoldAuthToken {} -> error "GetLegalHoldAuthToken: implement on demand (mockBrigAPIAccess)"
  AddLegalHoldClientToUserEither {} -> error "AddLegalHoldClientToUserEither: implement on demand (mockBrigAPIAccess)"
  RemoveLegalHoldClientFromUser {} -> error "RemoveLegalHoldClientFromUser: implement on demand (mockBrigAPIAccess)"
  GetAccountConferenceCallingConfigClient {} -> error "GetAccountConferenceCallingConfigClient: implement on demand (mockBrigAPIAccess)"
  GetLocalMLSClients {} -> error "GetLocalMLSClients: implement on demand (mockBrigAPIAccess)"
  GetLocalMLSClient {} -> error "GetLocalMLSClient: implement on demand (mockBrigAPIAccess)"
  UpdateSearchVisibilityInbound {} -> error "UpdateSearchVisibilityInbound: implement on demand (mockBrigAPIAccess)"
  GetUserExportData {} -> error "GetUserExportData: implement on demand (mockBrigAPIAccess)"
  DeleteBot {} -> error "DeleteBot: implement on demand (mockBrigAPIAccess)"
  GetAccountsBy {} -> error "GetAccountsBy: implement on demand (mockBrigAPIAccess)"
  GetUsersByVariousKeys {} -> error "GetUsersByVariousKeys: implement on demand (mockBrigAPIAccess)"
  CreateGroupInternal {} -> error "CreateGroupInternal: implement on demand (mockBrigAPIAccess)"
  GetGroupInternal {} -> error "GetGroupInternal: implement on demand (mockBrigAPIAccess)"
  GetGroupsInternal {} -> error "GetGroupsInternal: implement on demand (mockBrigAPIAccess)"
  UpdateGroup {} -> error "UpdateGroup: implement on demand (mockBrigAPIAccess)"
  DeleteGroupInternal {} -> error "DeleteGroupInternal: implement on demand (mockBrigAPIAccess)"
  DeleteApp {} -> error "DeleteApp: implement on demand (mockBrigAPIAccess)"
  GetAppIdsForTeam {} -> error "GetAppIdsForTeam: implement on demand (mockBrigAPIAccess)"
  SetAccountStatus {} -> error "SetAccountStatus: implement on demand (mockBrigAPIAccess)"
  CreateSAML {} -> error "CreateSAML: implement on demand (mockBrigAPIAccess)"
  CreateNoSAML {} -> error "CreateNoSAML: implement on demand (mockBrigAPIAccess)"
  UpdateEmail {} -> error "UpdateEmail: implement on demand (mockBrigAPIAccess)"
  GetAccount {} -> error "GetAccount: implement on demand (mockBrigAPIAccess)"
  GetAccountByHandle {} -> error "GetAccountByHandle: implement on demand (mockBrigAPIAccess)"
  GetByEmail {} -> error "GetByEmail: implement on demand (mockBrigAPIAccess)"
  SetName {} -> error "SetName: implement on demand (mockBrigAPIAccess)"
  SetHandle {} -> error "SetHandle: implement on demand (mockBrigAPIAccess)"
  SetManagedBy {} -> error "SetManagedBy: implement on demand (mockBrigAPIAccess)"
  DeletePendingEmailUpdate {} -> error "DeletePendingEmailUpdate: implement on demand (mockBrigAPIAccess)"
  SetSSOId {} -> error "SetSSOId: implement on demand (mockBrigAPIAccess)"
  SetRichInfo {} -> error "SetRichInfo: implement on demand (mockBrigAPIAccess)"
  SetLocale {} -> error "SetLocale: implement on demand (mockBrigAPIAccess)"
  GetRichInfo {} -> error "GetRichInfo: implement on demand (mockBrigAPIAccess)"
  CheckHandleAvailable {} -> error "CheckHandleAvailable: implement on demand (mockBrigAPIAccess)"
  SsoLogin {} -> error "SsoLogin: implement on demand (mockBrigAPIAccess)"
  GetStatus {} -> error "GetStatus: implement on demand (mockBrigAPIAccess)"
  GetStatusMaybe {} -> error "GetStatusMaybe: implement on demand (mockBrigAPIAccess)"
  SetStatus {} -> error "SetStatus: implement on demand (mockBrigAPIAccess)"
  GetDefaultUserLocale {} -> error "GetDefaultUserLocale: implement on demand (mockBrigAPIAccess)"
  CheckAdminGetTeamId {} -> error "CheckAdminGetTeamId: implement on demand (mockBrigAPIAccess)"
  SendSAMLIdPChangedEmail {} -> error "SendSAMLIdPChangedEmail: implement on demand (mockBrigAPIAccess)"
