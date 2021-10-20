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
module Test.Wire.API.Golden.Generated.QueuedNotification_user where

import Data.Aeson (Value (Array))
import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (fromJust)
import Wire.API.Notification (QueuedNotification, queuedNotification)

testObject_QueuedNotification_user_1 :: QueuedNotification
testObject_QueuedNotification_user_1 =
  ( queuedNotification
      (Id (fromJust (UUID.fromString "0000005f-0000-007b-0000-001a0000000a")))
      ( ( List1
            ( NonEmpty.fromList
                [ fromList [],
                  fromList [("\179372\&3", Array [])]
                ]
            )
        )
      )
  )
