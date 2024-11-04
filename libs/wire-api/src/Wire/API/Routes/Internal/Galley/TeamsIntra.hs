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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wire.API.Routes.Internal.Galley.TeamsIntra
  ( TeamStatus (..),
    TeamData (..),
    TeamStatusUpdate (..),
    TeamName (..),
    GuardLegalholdPolicyConflicts (..),
  )
where

import Data.Aeson
import Data.Currency qualified as Currency
import Data.Json.Util
import Data.OpenApi qualified as Swagger
import Data.Schema qualified as S
import Data.Time (UTCTime)
import Imports
import Test.QuickCheck.Arbitrary (Arbitrary)
import Wire.API.Message (UserClients)
import Wire.API.Team (Team)
import Wire.API.Team.LegalHold (LegalholdProtectee)
import Wire.Arbitrary (GenericUniform (..))

data TeamStatus
  = Active
  | PendingDelete
  | Deleted
  | Suspended
  | PendingActive
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform TeamStatus
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamStatus

instance S.ToSchema TeamStatus where
  schema =
    S.enum @Text "Access" $
      mconcat
        [ S.element "active" Active,
          S.element "pending_delete" PendingDelete,
          S.element "deleted" Deleted,
          S.element "suspended" Suspended,
          S.element "pending_active" PendingActive
        ]

data TeamData = TeamData
  { tdTeam :: !Team,
    tdStatus :: !TeamStatus,
    tdStatusTime :: !(Maybe UTCTime) -- This needs to be a Maybe due to backwards compatibility
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform TeamData
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamData

instance S.ToSchema TeamData where
  schema =
    S.object "TeamData" $
      TeamData
        <$> tdTeam S..= S.field "team" S.schema
        <*> tdStatus S..= S.field "status" S.schema
        <*> tdStatusTime S..= S.maybe_ (S.optField "status_time" utcTimeSchema)

data TeamStatusUpdate = TeamStatusUpdate
  { tuStatus :: !TeamStatus,
    tuCurrency :: !(Maybe Currency.Alpha)
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform TeamStatusUpdate
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamStatusUpdate

instance S.ToSchema TeamStatusUpdate where
  schema =
    S.object "TeamStatusUpdate" $
      TeamStatusUpdate
        <$> tuStatus S..= S.field "status" S.schema
        <*> tuCurrency S..= S.maybe_ (S.optField "currency" S.genericToSchema)

newtype TeamName = TeamName
  {tnName :: Text}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform TeamName
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamName

instance S.ToSchema TeamName where
  schema =
    S.object "TeamName" $
      TeamName
        <$> tnName S..= S.field "name" S.schema

data GuardLegalholdPolicyConflicts = GuardLegalholdPolicyConflicts
  { glhProtectee :: LegalholdProtectee,
    glhUserClients :: UserClients
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform GuardLegalholdPolicyConflicts)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema GuardLegalholdPolicyConflicts

instance S.ToSchema GuardLegalholdPolicyConflicts where
  schema =
    S.object "GuardLegalholdPolicyConflicts" $
      GuardLegalholdPolicyConflicts
        <$> glhProtectee S..= S.field "glhProtectee" S.schema
        <*> glhUserClients S..= S.field "glhUserClients" S.schema
