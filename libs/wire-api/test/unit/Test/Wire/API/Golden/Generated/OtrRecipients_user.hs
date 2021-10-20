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
module Test.Wire.API.Golden.Generated.OtrRecipients_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (fromJust)
import Wire.API.Message (OtrRecipients (..), UserClientMap (UserClientMap, userClientMap))

testObject_OtrRecipients_user_1 :: OtrRecipients
testObject_OtrRecipients_user_1 =
  OtrRecipients
    { otrRecipientsMap =
        UserClientMap
          { userClientMap =
              fromList
                [ ( Id (fromJust (UUID.fromString "00000025-0000-0031-0000-003e00000001")),
                    fromList
                      [ (ClientId {client = "10"}, "q"),
                        (ClientId {client = "4"}, "\f"),
                        (ClientId {client = "b"}, "\83295")
                      ]
                  ),
                  ( Id (fromJust (UUID.fromString "0000002c-0000-0078-0000-001d00000069")),
                    fromList [(ClientId {client = "1d"}, "\"\168226l"), (ClientId {client = "3"}, "{Pu^1")]
                  )
                ]
          }
    }
