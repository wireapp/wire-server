{-# OPTIONS_GHC -Wno-orphans #-}

module Federator.PolysemyOrphans where

import Control.Monad.Except (MonadError (..))
import Mu.Server (ServerError)
import Polysemy
import qualified Polysemy.Error as Polysemy

instance Member (Polysemy.Error ServerError) r => MonadError ServerError (Sem r) where
  throwError = Polysemy.throw
  catchError = Polysemy.catch
