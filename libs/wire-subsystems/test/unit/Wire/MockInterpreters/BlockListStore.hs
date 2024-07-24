module Wire.MockInterpreters.BlockListStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.BlockListStore
import Wire.UserKeyStore

inMemoryBlockListStoreInterpreter :: (Member (State [EmailKey]) r) => InterpreterFor BlockListStore r
inMemoryBlockListStoreInterpreter = interpret $ \case
  Insert uk -> modify (uk :)
  Exists uk -> gets (elem uk)
  Delete uk -> modify (filter (/= uk))
