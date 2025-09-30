{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.MockInterpreters.AppStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.AppStore

inMemoryAppStoreInterpreter ::
  forall r.
  (Member (State [StoredApp]) r) =>
  InterpreterFor AppStore r
inMemoryAppStoreInterpreter = interpret $ \case
  CreateApp app -> modify (app :)
  GetApp uid -> gets $ find $ \app -> app.id == uid
