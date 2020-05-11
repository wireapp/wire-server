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
import Control.Monad.Fail (MonadFail)
import Data.Aeson (decode)
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Test.Tasty.HUnit
import Util

executeSearch :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> m (Maybe (SearchResult Contact))
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
  return . decode . fromMaybe "" $ responseBody r

-- | ES is only refreshed occasionally; we don't want to wait for that in tests.
refreshIndex :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
refreshIndex brig =
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

reindex :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
reindex brig =
  post (brig . path "/i/index/reindex") !!! const 200 === statusCode

assertCanFind :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, MonadFail m, HasCallStack) => Brig -> UserId -> UserId -> Text -> m ()
assertCanFind brig self expected q = do
  Just r <- (fmap . fmap) searchResults $ executeSearch brig self q
  liftIO $ do
    assertBool ("No results for query: " <> show q) $
      not (null r)
    assertBool ("User not in results for query: " <> show q)
      $ elem expected . map contactUserId
      $ r

assertCan'tFind :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, MonadFail m, HasCallStack) => Brig -> UserId -> UserId -> Text -> m ()
assertCan'tFind brig self expected q = do
  Just r <- (fmap . fmap) searchResults $ executeSearch brig self q
  liftIO . assertBool ("User shouldn't be present in results for query: " <> show q)
    $ notElem expected . map contactUserId
    $ r
