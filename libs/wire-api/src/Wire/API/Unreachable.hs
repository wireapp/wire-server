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

-- | Types and utilies around unreachable backends and failing to process
-- various kinds of messages.
module Wire.API.Unreachable
  ( -- * Failed to process
    UnreachableUsers (unreachableUsers),
    unreachableFromList,
  )
where

import Data.Aeson qualified as A
import Data.Id
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NE
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Schema
import Imports

newtype UnreachableUsers = UnreachableUsers {unreachableUsers :: NonEmpty (Qualified UserId)}
  deriving stock (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema UnreachableUsers

instance Semigroup UnreachableUsers where
  (UnreachableUsers m) <> (UnreachableUsers n) = UnreachableUsers . NE.nub $ m <> n

instance ToSchema UnreachableUsers where
  schema =
    named "UnreachableUsers" $
      UnreachableUsers
        <$> unreachableUsers
          .= nonEmptyArray schema

unreachableFromList :: [Qualified UserId] -> Maybe UnreachableUsers
unreachableFromList = fmap (UnreachableUsers . NE.nub) . nonEmpty
