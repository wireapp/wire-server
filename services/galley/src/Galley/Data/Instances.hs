{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Data.Instances
  (
  )
where

import Cassandra.CQL
import Control.Error (note)
import Data.Domain (Domain, domainText, mkDomain)
import Galley.Types
import Galley.Types.Bot ()
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SearchVisibility
import Galley.Types.Teams.SSO
import Imports

deriving instance Cql MutedStatus

deriving instance Cql ReceiptMode

instance Cql ConvType where
  ctype = Tagged IntColumn

  toCql RegularConv = CqlInt 0
  toCql SelfConv = CqlInt 1
  toCql One2OneConv = CqlInt 2
  toCql ConnectConv = CqlInt 3

  fromCql (CqlInt i) = case i of
    0 -> return RegularConv
    1 -> return SelfConv
    2 -> return One2OneConv
    3 -> return ConnectConv
    n -> fail $ "unexpected conversation-type: " ++ show n
  fromCql _ = fail "conv-type: int expected"

instance Cql Access where
  ctype = Tagged IntColumn

  toCql PrivateAccess = CqlInt 1
  toCql InviteAccess = CqlInt 2
  toCql LinkAccess = CqlInt 3
  toCql CodeAccess = CqlInt 4

  fromCql (CqlInt i) = case i of
    1 -> return PrivateAccess
    2 -> return InviteAccess
    3 -> return LinkAccess
    4 -> return CodeAccess
    n -> fail $ "Unexpected Access value: " ++ show n
  fromCql _ = fail "Access value: int expected"

instance Cql AccessRole where
  ctype = Tagged IntColumn

  toCql PrivateAccessRole = CqlInt 1
  toCql TeamAccessRole = CqlInt 2
  toCql ActivatedAccessRole = CqlInt 3
  toCql NonActivatedAccessRole = CqlInt 4

  fromCql (CqlInt i) = case i of
    1 -> return PrivateAccessRole
    2 -> return TeamAccessRole
    3 -> return ActivatedAccessRole
    4 -> return NonActivatedAccessRole
    n -> fail $ "Unexpected AccessRole value: " ++ show n
  fromCql _ = fail "AccessRole value: int expected"

instance Cql ConvTeamInfo where
  ctype = Tagged $ UdtColumn "teaminfo" [("teamid", UuidColumn), ("managed", BooleanColumn)]

  toCql t = CqlUdt [("teamid", toCql (cnvTeamId t)), ("managed", toCql (cnvManaged t))]

  fromCql (CqlUdt u) = do
    t <- note "missing 'teamid' in teaminfo" ("teamid" `lookup` u) >>= fromCql
    m <- note "missing 'managed' in teaminfo" ("managed" `lookup` u) >>= fromCql
    pure (ConvTeamInfo t m)
  fromCql _ = fail "teaminfo: udt expected"

instance Cql TeamBinding where
  ctype = Tagged BooleanColumn

  toCql Binding = CqlBoolean True
  toCql NonBinding = CqlBoolean False

  fromCql (CqlBoolean True) = pure Binding
  fromCql (CqlBoolean False) = pure NonBinding
  fromCql _ = fail "teambinding: boolean expected"

instance Cql TeamStatus where
  ctype = Tagged IntColumn

  toCql Active = CqlInt 0
  toCql PendingDelete = CqlInt 1
  toCql Deleted = CqlInt 2
  toCql Suspended = CqlInt 3
  toCql PendingActive = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> return Active
    1 -> return PendingDelete
    2 -> return Deleted
    3 -> return Suspended
    4 -> return PendingActive
    n -> fail $ "unexpected team-status: " ++ show n
  fromCql _ = fail "team-status: int expected"

instance Cql SSOStatus where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ SSODisabled
    1 -> pure $ SSOEnabled
    _ -> fail "fromCql: Invalid SSOStatus"
  fromCql _ = fail "fromCql: SSOStatus: CqlInt expected"

  toCql SSODisabled = CqlInt 0
  toCql SSOEnabled = CqlInt 1

instance Cql CustomSearchVisibilityStatus where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ CustomSearchVisibilityDisabled
    1 -> pure $ CustomSearchVisibilityEnabled
    _ -> fail "fromCql: Invalid CustomSearchVisibilityStatus"
  fromCql _ = fail "fromCql: CustomSearchVisibilityStatus: CqlInt expected"

  toCql CustomSearchVisibilityDisabled = CqlInt 0
  toCql CustomSearchVisibilityEnabled = CqlInt 1

instance Cql CustomSearchVisibilityType where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ SearchVisibilityStandard
    1 -> pure $ SearchVisibilityNoNameOutsideTeam
    _ -> fail "fromCql: Invalid CustomSearchVisibilityType"
  fromCql _ = fail "fromCql: CustomSearchVisibilityType: CqlInt expected"

  toCql SearchVisibilityStandard = CqlInt 0
  toCql SearchVisibilityNoNameOutsideTeam = CqlInt 1

instance Cql Domain where
  ctype = Tagged TextColumn
  toCql = CqlText . domainText
  fromCql (CqlText txt) = either fail pure $ mkDomain txt
  fromCql _ = fail "Domain: Text expected"
