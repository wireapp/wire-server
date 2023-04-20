module Federator.EnvUpdater
  ( envUpdater
  ) where

envUpdater :: TVar Env -> IO ()
envUpdater tEnv = do
  pure ()
