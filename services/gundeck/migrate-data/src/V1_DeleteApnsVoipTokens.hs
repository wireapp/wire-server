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

module V1_DeleteApnsVoipTokens where

import Cassandra
import Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Data.Text qualified as Text
import Gundeck.DataMigration.Types
import Imports
import System.Logger.Class qualified as Log

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 1,
      text = "Delete APNS_VOIP push tokens",
      action =
        runConduit $
          zipSources
            (C.sourceList [(1 :: Int32) ..])
            getPushTokens
            .| C.mapM
              ( \(i, p) ->
                  Log.info (Log.field "push tokens" (show (i * pageSize)))
                    >> pure p
              )
            .| C.concatMap (filter isVoipToken)
            .| C.map (\(uid, token, app, transport, _mArn) -> (uid, token, app, transport))
            .| C.mapM_ deletePushToken
    }

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | We do not use the push token types here because they will likely be
-- changed in future breaking this migration.
getPushTokens ::
  (MonadClient m) =>
  ConduitM () [(UserId, Text, Text, Int32, Maybe Text)] m ()
getPushTokens = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (UserId, Text, Text, Int32, Maybe Text)
    cql = "SELECT usr, ptoken, app, transport, arn FROM user_push"

deletePushToken :: (MonadClient m) => (UserId, Text, Text, Int32) -> m ()
deletePushToken pair =
  retry x5 $ write cql (params LocalQuorum pair)
  where
    cql :: PrepQuery W (UserId, Text, Text, Int32) ()
    cql = "DELETE FROM user_push where usr = ? AND ptoken = ? AND app = ? AND transport = ?"

isVoipTransport :: Int32 -> Bool
isVoipTransport 3 = True -- APNS_VOIP
isVoipTransport 4 = True -- APNS_VOIP_SANDBOX
isVoipTransport _ = False

isVoipArn :: Text -> Bool
isVoipArn arn =
  case Text.splitOn ":" arn of
    ["arn", "aws", "sns", _region, _accountId, topic] ->
      case Text.splitOn "/" topic of
        ("endpoint" : "APNS_VOIP" : _) -> True
        ("endpoint" : "APNS_VOIP_SANDBOX" : _) -> True
        _ -> False
    _ -> False

isVoipToken :: (UserId, Text, Text, Int32, Maybe Text) -> Bool
isVoipToken (_, _, _, transport, mArn) =
  isVoipTransport transport || maybe False isVoipArn mArn
