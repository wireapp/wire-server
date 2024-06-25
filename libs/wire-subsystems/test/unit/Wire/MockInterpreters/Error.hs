module Wire.MockInterpreters.Error where

import Imports
import Polysemy
import Polysemy.Error

runErrorUnsafe :: (HasCallStack, Exception e) => InterpreterFor (Error e) r
runErrorUnsafe action = do
  res <- runError action
  case res of
    Left e -> error $ "Unexpected error: " <> displayException e
    Right x -> pure x

catchExpectedError :: (Member (Error e) r) => Sem r a -> Sem r (Maybe e)
catchExpectedError action = (const Nothing <$> action) `catch` (pure . Just)
