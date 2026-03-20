{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationSubsystem.Fetch
  ( getConversationIdsImpl,
  )
where

import Control.Error (lastMay)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Id
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Wire.API.Conversation (ConvIdsPage, ConversationPagingState)
import Wire.API.Routes.MultiTablePaging
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as ConvStore
import Wire.Sem.Paging.Cassandra (ResultSet (..), ResultSetType (..))

getConversationIdsResultSetImpl ::
  forall r.
  (Member ConversationStore r) =>
  Local UserId ->
  Range 1 1000 Int32 ->
  Maybe (Qualified ConvId) ->
  Sem r (ResultSet (Qualified ConvId))
getConversationIdsResultSetImpl lusr maxIds mLastId = do
  case fmap (flip relativeTo lusr) mLastId of
    Nothing -> getLocals Nothing
    Just (Local (tUnqualified -> lastId)) -> getLocals (Just lastId)
    Just (Remote lastId) -> getRemotes (Just lastId) maxIds
  where
    localDomain = tDomain lusr
    usr = tUnqualified lusr

    getLocals :: Maybe ConvId -> Sem r (ResultSet (Qualified ConvId))
    getLocals lastId = do
      localPage <- flip Qualified localDomain <$$> ConvStore.getLocalConversationIds usr lastId maxIds
      let remainingSize = fromRange maxIds - fromIntegral (length localPage.resultSetResult)
      case checked remainingSize of
        Nothing -> pure localPage {resultSetType = ResultSetTruncated}
        Just checkedRemaining -> do
          remotePage <- getRemotes Nothing checkedRemaining
          pure
            remotePage
              { resultSetResult = localPage.resultSetResult <> remotePage.resultSetResult
              }

    getRemotes :: Maybe (Remote ConvId) -> Range 1 1000 Int32 -> Sem r (ResultSet (Qualified ConvId))
    getRemotes lastRemote maxRemotes = tUntagged <$$> ConvStore.getRemoteConversationIds usr lastRemote maxRemotes

-- | This function only exists because we use the 'MultiTablePage' type for the
-- endpoint. Since now the pagination is based on the qualified ids, we can
-- remove the use of this type in future API versions.
getConversationIdsImpl ::
  forall r.
  (Member ConversationStore r) =>
  Local UserId ->
  Range 1 1000 Int32 ->
  Maybe ConversationPagingState ->
  Sem r ConvIdsPage
getConversationIdsImpl lusr maxIds pagingState = do
  let mLastId = Aeson.decode . BS.fromStrict =<< (.mtpsState) =<< pagingState
  resultSet <- getConversationIdsResultSetImpl lusr maxIds mLastId
  let mLastResult = lastMay resultSet.resultSetResult
  pure
    MultiTablePage
      { mtpResults = resultSet.resultSetResult,
        mtpHasMore = case resultSet.resultSetType of
          ResultSetTruncated -> True
          ResultSetComplete -> False,
        mtpPagingState =
          MultiTablePagingState
            { mtpsTable = case fmap (flip relativeTo lusr) mLastResult of
                Just (Local _) -> PagingLocals
                Just (Remote _) -> PagingRemotes
                Nothing -> PagingRemotes,
              mtpsState = BS.toStrict . Aeson.encode <$> mLastResult
            }
      }
