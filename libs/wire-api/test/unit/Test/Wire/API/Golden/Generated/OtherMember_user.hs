-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.OtherMember_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation (OtherMember (..))
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_OtherMember_user_1 :: OtherMember
testObject_OtherMember_user_1 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000008-0000-0009-0000-000f00000001"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000004")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))
              }
          ),
      omConvRoleName = fromJust (parseRoleName "kd8736")
    }

testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001f-0000-000c-0000-001c0000000f"))) domain,
      omService = Nothing,
      omConvRoleName = fromJust (parseRoleName "y9z93u3kbwt873eghekqgmy0ho8hgrtlo3f5e6nq9icedmjbzx7ao0ycr5_gyunq4uuw")
    }
