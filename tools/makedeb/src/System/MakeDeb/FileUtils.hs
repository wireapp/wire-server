{-# LANGUAGE BangPatterns #-}

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
