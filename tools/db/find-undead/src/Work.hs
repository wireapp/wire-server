{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module Work where

import Cassandra
import Conduit
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import qualified Data.Text.Encoding as Text
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import Wire.API.User (UserSSOId)

runCommand :: Logger -> ClientState -> IO ()
runCommand l brig = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient brig) getSSOIds)
      .| C.mapM_
        ( \(i, usersAndSSOIds) -> do
            Log.info l (Log.field "number of ssoIds processed" (show (i * pageSize)))
            mapM_ (uncurry (validateSSOId l)) usersAndSSOIds
        )

pageSize :: Int32
pageSize = 2000

getSSOIds :: ConduitM () [(UserId, Maybe Text)] Client ()
getSSOIds = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (UserId, Maybe Text)
    cql = "select id, sso_id from user"

validateSSOId :: Logger -> UserId -> Maybe Text -> IO ()
validateSSOId _ _ Nothing = pure ()
validateSSOId l uid (Just sso) = do
  let decoded = eitherDecode @UserSSOId . LBS.fromStrict . Text.encodeUtf8 $ sso
  case decoded of
    Right _ -> pure ()
    Left err -> Log.err l $ Log.field "user" (idToText uid) . Log.field "sso_id" sso . Log.msg err
