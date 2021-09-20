{-# LANGUAGE OverloadedLists #-}

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
module Test.Wire.API.Golden.Generated.LimitedQualifiedUserIdList_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified, qDomain, qUnqualified))
import Data.Range (unsafeRange)
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.User (LimitedQualifiedUserIdList (..))

testObject_LimitedQualifiedUserIdList_user_1 :: LimitedQualifiedUserIdList 1
testObject_LimitedQualifiedUserIdList_user_1 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        unsafeRange
          [ Qualified
              { qUnqualified = Id (fromJust (UUID.fromString "00005bed-0000-0771-0000-447a00005b32")),
                qDomain = Domain {_domainText = "sh7636.gcg-c"}
              }
          ]
    }

testObject_LimitedQualifiedUserIdList_user_2 :: LimitedQualifiedUserIdList 2
testObject_LimitedQualifiedUserIdList_user_2 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        unsafeRange
          [ Qualified
              { qUnqualified = Id (fromJust (UUID.fromString "000016a5-0000-000e-0000-2ad50000589c")),
                qDomain = Domain {_domainText = "t02vm1.gm810xwh1l4rb"}
              },
            Qualified
              { qUnqualified = Id (fromJust (UUID.fromString "00004bd5-0000-363b-0000-1af6000051cf")),
                qDomain = Domain {_domainText = "620au-6.2j.x23-5w"}
              }
          ]
    }

testObject_LimitedQualifiedUserIdList_user_3 :: LimitedQualifiedUserIdList 3
testObject_LimitedQualifiedUserIdList_user_3 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        unsafeRange
          [ Qualified
              { qUnqualified = Id (fromJust (UUID.fromString "0000027a-0000-42bd-0000-2fdb00000811")),
                qDomain = Domain {_domainText = "06s.eaq.xbih-26z--5.jqaqc"}
              }
          ]
    }
