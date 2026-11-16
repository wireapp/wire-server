{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserPendingActivationStore where

import Data.Id
import Data.Time.Clock
import Imports
import Polysemy

data UserPendingActivation = UserPendingActivation
  { upaUserId :: !UserId,
    upaDay :: !UTCTime
  }
  deriving stock (Eq, Show, Ord)

data UserPendingActivationStore m a where
  Add :: UserPendingActivation -> UserPendingActivationStore m ()
  -- | @ListPage after limit@ returns up to @limit@ entries following the entry
  -- of user @after@ (from the start for 'Nothing') in the store's iteration
  -- order, and whether more entries follow. @after@ need not be stored
  -- (anymore), so callers may remove entries between pages.
  ListPage :: Maybe UserId -> Int32 -> UserPendingActivationStore m ([UserPendingActivation], Bool)
  RemoveMultiple :: [UserId] -> UserPendingActivationStore m ()

makeSem ''UserPendingActivationStore

remove :: (Member UserPendingActivationStore r) => UserId -> Sem r ()
remove uid = removeMultiple [uid]

-- | Visit every entry page by page, continuing after the last entry of the
-- previous page. @f@ may remove the entries it is given.
forEachPage ::
  (Monad m) =>
  (Maybe UserId -> m ([UserPendingActivation], Bool)) ->
  ([UserPendingActivation] -> m ()) ->
  m ()
forEachPage getPage f = go Nothing
  where
    go after = do
      (items, more) <- getPage after
      f items
      case reverse items of
        l : _ | more -> go (Just l.upaUserId)
        _ -> pure ()
