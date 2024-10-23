{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Manual.QualifiedUserClientPrekeyMap where

import Data.Domain
import GHC.Exts (IsList (fromList))
import Imports
import Test.Wire.API.Golden.Manual.UserClientPrekeyMap
import Wire.API.User.Client (QualifiedUserClientPrekeyMap, mkQualifiedUserClientPrekeyMap)

testObject_QualifiedUserClientPrekeyMap_1 :: QualifiedUserClientPrekeyMap
testObject_QualifiedUserClientPrekeyMap_1 = mkQualifiedUserClientPrekeyMap mempty

testObject_QualifiedUserClientPrekeyMap_2 :: QualifiedUserClientPrekeyMap
testObject_QualifiedUserClientPrekeyMap_2 =
  mkQualifiedUserClientPrekeyMap . fromList $
    [ (Domain "alpha.example.com", testObject_UserClientPrekeyMap_1),
      (Domain "beta.example.com", testObject_UserClientPrekeyMap_2),
      (Domain "gamma.example.com", testObject_UserClientPrekeyMap_3),
      (Domain "delta.example.com", testObject_UserClientPrekeyMap_4),
      (Domain "epsilon.example.com", testObject_UserClientPrekeyMap_5),
      (Domain "zeta.example.com", testObject_UserClientPrekeyMap_6),
      (Domain "eta.example.com", testObject_UserClientPrekeyMap_7),
      (Domain "theta.example.com", testObject_UserClientPrekeyMap_8),
      (Domain "meta.example.com", testObject_UserClientPrekeyMap_8)
    ]
