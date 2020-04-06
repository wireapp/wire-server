{-# LANGUAGE BangPatterns #-}

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

module System.MakeDeb.FileUtils where

import Imports
import System.FilePath

foldFiles :: (Functor m, MonadIO m) => (a -> FilePath -> m a) -> a -> FilePath -> m a
foldFiles f !a !d = do
  c <- liftIO $ check d
  case c of
    (True, True, False) -> f a d
    (True, False, True) -> foldDir
    _ -> return a
  where
    foldDir = do
      xs <- map (d </>) . filter dots <$> liftIO (getDirectoryContents d)
      foldM (foldFiles f) a xs
    dots "." = False
    dots ".." = False
    dots _ = True
    check x =
      (,,)
        <$> (readable <$> getPermissions x)
        <*> doesFileExist x
        <*> doesDirectoryExist x

traverseFiles :: (Functor m, MonadIO m) => (FilePath -> m ()) -> FilePath -> m ()
traverseFiles f = foldFiles (\_ n -> f n) ()
