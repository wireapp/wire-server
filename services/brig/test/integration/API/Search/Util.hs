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

module API.Search.Util where

import Bilge
import Bilge.Assert
import Control.Monad.Catch (MonadCatch)
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Conversion.To (toByteString)
import Data.Domain (Domain)
import Data.Id
import Data.Qualified (Qualified (..))
import Data.Range (Range)
import Data.String.Conversions
import Data.Text.Encoding (encodeUtf8)
import Database.Bloodhound qualified as ES
import Imports
import Network.HTTP.Client qualified as HTTP
import Test.Tasty.HUnit
import Util
import Wire.API.User
import Wire.API.User.Search

executeSearch :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> m (SearchResult Contact)
executeSearch brig self term = executeSearch' brig self term Nothing Nothing

executeSearchWithDomain :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> Domain -> m (SearchResult Contact)
executeSearchWithDomain brig self term domain = executeSearch' brig self term (Just domain) Nothing

executeSearch' :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> Maybe Domain -> Maybe Int -> m (SearchResult Contact)
executeSearch' brig self q maybeDomain maybeSize = do
  r <-
    searchRequest brig self q maybeDomain maybeSize
      <!! const 200 === statusCode
  responseJsonError r

searchRequest :: (MonadHttp m) => Brig -> UserId -> Text -> Maybe Domain -> Maybe Int -> m ResponseLBS
searchRequest brig self q maybeDomain maybeSize = do
  get
    ( brig
        . path "/search/contacts"
        . zUser self
        . queryItem "q" (encodeUtf8 q)
        . maybe id (queryItem "domain" . toByteString') maybeDomain
        . maybe id (queryItem "size" . toByteString') maybeSize
    )

-- | ES is only refreshed occasionally; we don't want to wait for that in tests.
refreshIndex :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
refreshIndex brig =
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

reindex :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
reindex brig =
  post (brig . path "/i/index/reindex") !!! const 200 === statusCode

assertCanFindByName :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> m ()
assertCanFindByName brig self expected =
  assertCanFind brig (userId self) (userQualifiedId expected) (fromName $ userDisplayName expected)

assertCan'tFindByName :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> User -> m ()
assertCan'tFindByName brig self expected =
  assertCan'tFind brig (userId self) (userQualifiedId expected) (fromName $ userDisplayName expected)

assertCanFind :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Qualified UserId -> Text -> m ()
assertCanFind brig self expected q = do
  r <- searchResults <$> executeSearch brig self q
  liftIO $ do
    assertBool ("No results for query: " <> show q) $
      not (null r)
    assertBool ("User not in results for query: " <> show q) $
      expected `elem` map contactQualifiedId r

assertCanFindWithDomain :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Qualified UserId -> Text -> Domain -> m ()
assertCanFindWithDomain brig self expected q domain = do
  r <- searchResults <$> executeSearchWithDomain brig self q domain
  liftIO $ do
    assertBool ("No results for query: " <> show q) $
      not (null r)
    assertBool ("User not in results for query: " <> show q) $
      expected `elem` map contactQualifiedId r

assertCan'tFind :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Qualified UserId -> Text -> m ()
assertCan'tFind brig self expected q = do
  r <- searchResults <$> executeSearch brig self q
  liftIO $ do
    assertBool ("User shouldn't be present in results for query: " <> show q) $
      expected `notElem` map contactQualifiedId r

assertCan'tFindWithDomain :: (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Qualified UserId -> Text -> Domain -> m ()
assertCan'tFindWithDomain brig self expected q domain = do
  r <- searchResults <$> executeSearchWithDomain brig self q domain
  liftIO $ do
    assertBool ("User shouldn't be present in results for query: " <> show q) $
      expected `notElem` map contactQualifiedId r

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
executeTeamUserSearch brig teamid self mbSearchText mRoleFilter mSortBy mSortOrder =
  executeTeamUserSearchWithMaybeState brig teamid self mbSearchText mRoleFilter mSortBy mSortOrder Nothing Nothing

executeTeamUserSearchWithMaybeState ::
  (MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  TeamId ->
  UserId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Maybe (Range 1 500 Int32) ->
  Maybe PagingState ->
  m (SearchResult TeamContact)
executeTeamUserSearchWithMaybeState brig teamid self mbSearchText mRoleFilter mSortBy mSortOrder mSize mPagingState = do
  r <-
    get
      ( brig
          . paths ["/teams", toByteString' teamid, "search"]
          . zUser self
          . maybe id (queryItem "q" . encodeUtf8) mbSearchText
          . maybe id (queryItem "frole" . cs . toByteString) mRoleFilter
          . maybe id (queryItem "sortby" . cs . toByteString) mSortBy
          . maybe id (queryItem "sortorder" . cs . toByteString) mSortOrder
          . maybe id (queryItem "pagingState" . cs . toByteString) mPagingState
          . maybe id (queryItem "size" . cs . toByteString) mSize
      )
      <!! const 200
        === statusCode
  responseJsonError r

mkBHEnv :: Text -> HTTP.Manager -> ES.BHEnv
mkBHEnv url mgr = do
  (ES.mkBHEnv (ES.Server url) mgr) {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername "elastic") (ES.EsPassword "changeme")}
