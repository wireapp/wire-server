{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Wire.API.Team where

import Data.Aeson
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import qualified Wire.API.Team.Member as Team.Member

-- NB: validateEveryToJSON from servant-swagger doesn't render these tests unnecessary!

tests :: TestTree
tests =
  testGroup
    "Common (types vs. aeson)"
    [ testCase "{} is a valid TeamMemberDeleteData" $ do
        assertBool "{}" (isRight (eitherDecode @Team.Member.TeamMemberDeleteData "{}"))
    ]
