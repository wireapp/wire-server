module Wire.MockInterpreters.Error where

import Imports
import Polysemy
import Polysemy.Error

-- TODO: unify with mini backend
runErrorUnsafe :: (HasCallStack, Exception e) => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x
