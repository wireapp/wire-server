{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

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

module API
  ( tests,
  )
where

import Bilge
import Brig.Types.Intra
import Control.Applicative
import Control.Lens hiding ((.=))
import Data.ByteString.Conversion
import Data.Id
import Imports
import Stern.Types
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util
import Wire.API.User

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API tests"
    [ test s "GET /users/by-ids" testGetUsersByIds,
      test s "GET /teams/:id" testGetTeamInfo
    ]

testGetUsersByIds :: TestM ()
testGetUsersByIds = do
  uid <- randomUser
  [userAccount] <- getUsersByIds uid
  liftIO $ userAccount.accountUser.userId @?= uid

testGetTeamInfo :: TestM ()
testGetTeamInfo = do
  (_, tid, _) <- createBindingTeamWithNMembers 10
  info <- getTeamInfo tid
  liftIO $ length info.tiMembers @?= 11

-------------------------------------------------------------------------------
-- API Calls

getUsersByIds :: UserId -> TestM [UserAccount]
getUsersByIds uid = do
  stern <- view tsStern
  r <-
    get
      ( stern
          . paths ["users", "by-ids"]
          . queryItem "ids" (toByteString' uid)
          . expect2xx
      )
  pure $ responseJsonUnsafe r

getTeamInfo :: TeamId -> TestM TeamInfo
getTeamInfo tid = do
  stern <- view tsStern
  r <-
    get
      ( stern
          . paths ["teams", toByteString' tid]
          . expect2xx
      )
  pure $ responseJsonUnsafe r
