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

module API.Search.Util where

import Bilge
import Bilge.Assert
import Brig.Types
import Control.Monad.Catch (MonadCatch)
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Conversion.To (toByteString)
import Data.Id
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Imports
import Test.Tasty.HUnit
import Util
import Data.Qualified (Qualified(..))
import Wire.API.User.Search (RoleFilter (..), TeamContact (..), TeamUserSearchSortBy, TeamUserSearchSortOrder)

executeSearch :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> m (SearchResult Contact)
executeSearch brig self q = do
  r <-
    get
      ( brig
          . path "/search/contacts"
          . zUser self
          . queryItem "q" (encodeUtf8 q)
      )
      <!! const 200
      === statusCode
  responseJsonError r

-- | ES is only refreshed occasionally; we don't want to wait for that in tests.
refreshIndex :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
refreshIndex brig =
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

reindex :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
reindex brig =
  post (brig . path "/i/index/reindex") !!! const 200 === statusCode

assertCanFindByName :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> m ()
assertCanFindByName brig self expected =
  assertCanFind brig (userId self) (userId expected) (fromName $ userDisplayName expected)

assertCan'tFindByName :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> m ()
assertCan'tFindByName brig self expected =
  assertCan'tFind brig (userId self) (userId expected) (fromName $ userDisplayName expected)

assertCanFind :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> UserId -> Text -> m ()
assertCanFind brig self expected q = do
  r <- searchResults <$> executeSearch brig self q
  liftIO $ do
    assertBool ("No results for query: " <> show q) $
      not (null r)
    assertBool ("User not in results for query: " <> show q) $
      expected `elem` (map contactUserId r)

assertCan'tFind :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> UserId -> Text -> m ()
assertCan'tFind brig self expected q = do
  r <- searchResults <$> executeSearch brig self q
  liftIO $ do
    assertBool ("User shouldn't be present in results for query: " <> show q) $
      expected `notElem` map contactUserId r

contactUserId :: Contact -> UserId
contactUserId = qUnqualified . contactQualifiedId

executeTeamUserSearch ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  TeamId ->
  UserId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  m (SearchResult TeamContact)
executeTeamUserSearch brig teamid self mbSearchText mRoleFilter mSortBy mSortOrder = do
  r <-
    get
      ( brig
          . paths ["/teams", toByteString' teamid, "search"]
          . zUser self
          . maybe id (queryItem "q" . encodeUtf8) mbSearchText
          . maybe id (queryItem "frole" . cs . toByteString) mRoleFilter
          . maybe id (queryItem "sortby" . cs . toByteString) mSortBy
          . maybe id (queryItem "sortorder" . cs . toByteString) mSortOrder
      )
      <!! const 200
      === statusCode
  responseJsonError r
