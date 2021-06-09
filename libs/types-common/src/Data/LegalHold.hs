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

module Data.LegalHold where

import Cassandra.CQL
import Control.Lens ((?~))
import Data.Aeson hiding (constructorTagModifier)
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Test.QuickCheck

data UserLegalHoldStatus
  = UserLegalHoldDisabled
  | UserLegalHoldPending
  | UserLegalHoldEnabled
  | UserLegalHoldNoConsent
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema UserLegalHoldStatus

instance ToSchema UserLegalHoldStatus where
  schema =
    (S.schema . description ?~ desc) $
      enum @Text "UserLegalHoldStatus" $
        element "enabled" UserLegalHoldEnabled
          <> element "pending" UserLegalHoldPending
          <> element "disabled" UserLegalHoldDisabled
          <> element "no_consent" UserLegalHoldNoConsent
    where
      desc =
        "states whether a user is under legal hold, "
          <> "or whether legal hold is pending approval."

defUserLegalHoldStatus :: UserLegalHoldStatus
defUserLegalHoldStatus = UserLegalHoldNoConsent

typeUserLegalHoldStatus :: Doc.DataType
typeUserLegalHoldStatus =
  Doc.string $
    Doc.enum
      [ "enabled",
        "pending",
        "disabled",
        "no_consent"
      ]

instance Cql UserLegalHoldStatus where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure UserLegalHoldDisabled
    1 -> pure UserLegalHoldPending
    2 -> pure UserLegalHoldEnabled
    3 -> pure UserLegalHoldNoConsent
    _ -> Left "fromCql: Invalid UserLegalHoldStatus"
  fromCql _ = Left "fromCql: UserLegalHoldStatus: CqlInt expected"

  toCql UserLegalHoldDisabled = CqlInt 0
  toCql UserLegalHoldPending = CqlInt 1
  toCql UserLegalHoldEnabled = CqlInt 2
  toCql UserLegalHoldNoConsent = CqlInt 3

instance Arbitrary UserLegalHoldStatus where
  arbitrary = elements [minBound ..]
