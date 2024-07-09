module Wire.MockInterpreters.PropertyStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Properties
import Wire.PropertyStore

inMemoryPropertyStoreInterpreter :: (Member (State (Map UserId (Map PropertyKey RawPropertyValue))) r) => InterpreterFor PropertyStore r
inMemoryPropertyStoreInterpreter = interpret $ \case
  InsertProperty u k v -> modify $ Map.insertWith (Map.union) u (Map.singleton k v)
  LookupProperty u k -> gets $ Map.lookup k <=< Map.lookup u
  CountProperties u -> gets $ Map.size . Map.findWithDefault mempty u
  DeleteProperty u k -> modify $ Map.adjust (Map.delete k) u
  ClearProperties u -> modify $ Map.delete u
  GetPropertyKeys u -> gets $ Map.keys . Map.findWithDefault mempty u
  GetAllProperties u -> gets $ Map.toAscList . Map.findWithDefault mempty u
