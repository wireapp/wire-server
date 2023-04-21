module Federator.EnvUpdater
  ( envUpdater
  ) where

import Control.Concurrent.STM (TVar)
import Federator.Env (Env)
import Prelude (IO, pure, ($))
import Control.Monad (forever)

envUpdater :: TVar Env -> IO ()
envUpdater _tEnv = do
  -- TODO: This needs to update the TVar with a new Env value on some signal.
  -- Presumably, configuration for how this can be done will be available in
  -- the Env values being initially passed in.
  forever $ do
    pure ()