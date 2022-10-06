{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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

module Galley.Types.Teams.Intra
  ( TeamStatus (..),
    TeamData (..),
    TeamStatusUpdate (..),
    TeamName (..),
    GuardLegalholdPolicyConflicts (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Currency as Currency
import Data.Json.Util
import qualified Data.Schema as S
import qualified Data.Swagger as Swagger hiding (schema)
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
    -- TODO: Remove Currency selection once billing supports currency changes after team creation
  }
  deriving (Eq, Show, Generic)

instance FromJSON TeamStatusUpdate where
  parseJSON = withObject "team-status-update" $ \o ->
    TeamStatusUpdate <$> o .: "status"
      <*> o .:? "currency"

instance ToJSON TeamStatusUpdate where
  toJSON s =
    object
      [ "status" .= tuStatus s,
        "currency" .= tuCurrency s
      ]

newtype TeamName = TeamName
  {tnName :: Text}
  deriving (Eq, Show, Generic)

deriveJSON toJSONFieldName ''TeamName

data GuardLegalholdPolicyConflicts = GuardLegalholdPolicyConflicts
  { glhProtectee :: LegalholdProtectee,
    glhUserClients :: UserClients
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform GuardLegalholdPolicyConflicts)

instance ToJSON GuardLegalholdPolicyConflicts

instance FromJSON GuardLegalholdPolicyConflicts
