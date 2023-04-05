{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import qualified Data.Set as Set
import Data.String.Conversions
import Imports
import Stern.API.Routes (UserConnectionGroups (..))
import Stern.Types
import Test.Tasty
import Test.Tasty.HUnit
import TestSetup
import Util
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.User

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "API tests"
    [ test s "GET /i/status" testGetStatus,
      test s "POST /users/:uid/suspend" testSuspendUser,
      test s "POST /users/:uid/unsuspend" testUnsuspendUser,
      test s "GET /users/by-email" testGetUsersByEmail,
      test s "GET /users/by-phone" testGetUsersByPhone,
      test s "GET /users/by-ids" testGetUsersByIds,
      test s "GET /users/by-handles" testGetUsersByHandles,
      test s "GET /users/:id/connections" testGetConnections,
      test s "GET /users/connections?ids=..." testGetConnectionsByIds,
      test s "GET /teams/:id" testGetTeamInfo
    ]

testGetConnectionsByIds :: TestM ()
testGetConnectionsByIds = do
  uids <- sequence [randomUser, randomUser, randomUser]
  connections <- getConnectionsByUserIds uids
  liftIO $ connections @?= []

testGetConnections :: TestM ()
testGetConnections = do
  uid <- randomUser
  connections <- getConnections uid
  liftIO $ connections @?= UserConnectionGroups 0 0 0 0 0 0 0

testGetUsersByHandles :: TestM ()
testGetUsersByHandles = do
  uid <- randomUser
  h <- randomHandle
  void $ setHandle uid h
  [ua] <- getUsersByHandles h
  liftIO $ ua.accountUser.userId @?= uid

testGetUsersByPhone :: TestM ()
testGetUsersByPhone = do
  (uid, phone) <- randomPhoneUser
  [ua] <- getUsersByPhone phone
  liftIO $ ua.accountUser.userId @?= uid

testGetUsersByEmail :: TestM ()
testGetUsersByEmail = do
  (uid, email) <- randomEmailUser
  [ua] <- getUsersByEmail email
  liftIO $ ua.accountUser.userId @?= uid

testUnsuspendUser :: TestM ()
testUnsuspendUser = do
  uid <- randomUser
  void $ postSupendUser uid
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.accountStatus @?= Suspended
  void $ postUnsuspendUser uid
  do
    [ua] <- getUsersByIds [uid]
    liftIO $ ua.accountStatus @?= Active

testSuspendUser :: TestM ()
testSuspendUser = do
  uid <- randomUser
  void $ postSupendUser uid
  [ua] <- getUsersByIds [uid]
  liftIO $ ua.accountStatus @?= Suspended

testGetStatus :: TestM ()
testGetStatus = do
  r <- getStatus
  liftIO $ do
    statusCode r @?= 200

testGetUsersByIds :: TestM ()
testGetUsersByIds = do
  uid1 <- randomUser
  uid2 <- randomUser
  uas <- getUsersByIds [uid1, uid2]
  liftIO $ do
    length uas @?= 2
    Set.fromList ((.accountUser.userId) <$> uas) @?= Set.fromList [uid1, uid2]

testGetTeamInfo :: TestM ()
testGetTeamInfo = do
  (_, tid, _) <- createBindingTeamWithNMembers 10
  info <- getTeamInfo tid
  liftIO $ length info.tiMembers @?= 11

-------------------------------------------------------------------------------
-- API Calls

instance (ToByteString a) => ToByteString [a] where
  builder xs = builder $ cs @String @ByteString $ intercalate "," (cs . toByteString' <$> xs)

getConnectionsByUserIds :: [UserId] -> TestM [ConnectionStatus]
getConnectionsByUserIds uids = do
  s <- view tsStern
  r <-
    get
      ( s
          . paths ["users", "connections"]
          . queryItem "ids" (toByteString' uids)
          . expect2xx
      )
  pure $ responseJsonUnsafe r

getConnections :: UserId -> TestM UserConnectionGroups
getConnections uid = do
  s <- view tsStern
  r <- get (s . paths ["users", toByteString' uid, "connections"] . expect2xx)
  pure $ responseJsonUnsafe r

getUsersByHandles :: Text -> TestM [UserAccount]
getUsersByHandles h = do
  stern <- view tsStern
  r <-
    get
      ( stern
          . paths ["users", "by-handles"]
          . queryItem "handles" (cs h)
          . expect2xx
      )
  pure $ responseJsonUnsafe r

getUsersByPhone :: Phone -> TestM [UserAccount]
getUsersByPhone phone = do
  stern <- view tsStern
  r <-
    get
      ( stern
          . paths ["users", "by-phone"]
          . queryItem "phone" (toByteString' phone)
          . expect2xx
      )
  pure $ responseJsonUnsafe r

getUsersByEmail :: Email -> TestM [UserAccount]
getUsersByEmail email = do
  stern <- view tsStern
  r <-
    get
      ( stern
          . paths ["users", "by-email"]
          . queryItem "email" (toByteString' email)
          . expect2xx
      )
  pure $ responseJsonUnsafe r

postUnsuspendUser :: UserId -> TestM ResponseLBS
postUnsuspendUser uid = do
  stern <- view tsStern
  post
    ( stern
        . paths ["users", toByteString' uid, "unsuspend"]
        . expect2xx
    )

postSupendUser :: UserId -> TestM ResponseLBS
postSupendUser uid = do
  stern <- view tsStern
  post
    ( stern
        . paths ["users", toByteString' uid, "suspend"]
        . expect2xx
    )

getStatus :: TestM ResponseLBS
getStatus = do
  stern <- view tsStern
  get (stern . paths ["i", "status"] . expect2xx)

getUsersByIds :: [UserId] -> TestM [UserAccount]
getUsersByIds uids = do
  stern <- view tsStern
  r <-
    get
      ( stern
          . paths ["users", "by-ids"]
          . queryItem "ids" (toByteString' uids)
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
