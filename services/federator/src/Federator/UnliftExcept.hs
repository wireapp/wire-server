{-# OPTIONS_GHC -Wno-orphans #-}

module Federator.UnliftExcept where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Imports
import UnliftIO.Exception

-- TODO: Check if this is actually sane
-- TODO: See if we can rid RPC of MonadUnliftIO
instance (MonadUnliftIO m, Exception e) => MonadUnliftIO (ExceptT e m) where
  withRunInIO exceptToIO = ExceptT $
    try $ do
      withRunInIO $ \runInIO ->
        exceptToIO (runInIO . (either throwIO pure <=< runExceptT))
