{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Channels where

import API.Galley (CreateConv (groupConvType), defMLS, postConversation)
import GHC.Stack
import MLS.Util (createMLSClient, uploadNewKeyPackage)
import SetupHelpers (createTeam)
import Testlib.JSON
import Testlib.Prelude

-- TODO: user must be member or external member of a team
-- TODO: user must have permission to create a channel
-- TODO: feature flag must be enabled
-- TODO: must be MLS conversation
testCreateChannel :: (HasCallStack) => App ()
testCreateChannel = do
  (owner, _tid, mems) <- createTeam OwnDomain 3
  mlsClients@(ownerClient : _) <- traverse (createMLSClient def def) (owner : mems)
  for_ mlsClients (uploadNewKeyPackage def)
  conv <- postConversation ownerClient defMLS {groupConvType = Just "channel"} >>= getJSON 201
  conv %. "group_conv_type" `shouldMatch` "channel"
