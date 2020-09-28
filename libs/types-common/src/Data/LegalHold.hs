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
import Data.Aeson
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import Imports
import Test.QuickCheck

data UserLegalHoldStatus
  = UserLegalHoldDisabled
  | UserLegalHoldPending
  | UserLegalHoldEnabled
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

typeUserLegalHoldStatus :: Doc.DataType
typeUserLegalHoldStatus =
  Doc.string $
    Doc.enum
      [ "enabled",
        "pending",
        "disabled"
      ]

instance ToJSON UserLegalHoldStatus where
  toJSON UserLegalHoldDisabled = "disabled"
  toJSON UserLegalHoldPending = "pending"
  toJSON UserLegalHoldEnabled = "enabled"

instance FromJSON UserLegalHoldStatus where
  parseJSON = withText "UserLegalHoldStatus" $ \case
    "disabled" -> pure UserLegalHoldDisabled
    "pending" -> pure UserLegalHoldPending
    "enabled" -> pure UserLegalHoldEnabled
    x -> fail $ "unexpected status type: " <> T.unpack x

instance Cql UserLegalHoldStatus where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ UserLegalHoldDisabled
    1 -> pure $ UserLegalHoldPending
    2 -> pure $ UserLegalHoldEnabled
    _ -> Left "fromCql: Invalid UserLegalHoldStatus"
  fromCql _ = Left "fromCql: UserLegalHoldStatus: CqlInt expected"

  toCql UserLegalHoldDisabled = CqlInt 0
  toCql UserLegalHoldPending = CqlInt 1
  toCql UserLegalHoldEnabled = CqlInt 2

instance Arbitrary UserLegalHoldStatus where
  arbitrary = elements [minBound ..]
