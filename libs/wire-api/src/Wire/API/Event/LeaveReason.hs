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

module Wire.API.Event.LeaveReason where

import Data.Schema
import Imports
import Wire.Arbitrary

--  | The reason for a member to leave
--    There are three reasons
--    - the member has left on their own
--    - the member was removed from the team
--    - the member was removed by another member
data EdMemberLeftReason
  = -- | The member has left on their own
    EdReasonLeft
  | -- | The member was removed from the team and/or deleted
    EdReasonDeleted
  | -- | The member was removed by another member
    EdReasonRemoved
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform EdMemberLeftReason

instance ToSchema EdMemberLeftReason where
  schema =
    enum @Text "EdMemberLeftReason" $
      mconcat
        [ element "left" EdReasonLeft,
          element "user-deleted" EdReasonDeleted,
          element "removed" EdReasonRemoved
        ]
