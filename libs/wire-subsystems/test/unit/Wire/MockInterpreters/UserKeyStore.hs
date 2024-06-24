module Wire.MockInterpreters.UserKeyStore where

import Data.Id
import Data.Map qualified as M
import Imports
import Polysemy
import Polysemy.State
import Wire.UserKeyStore

staticUserKeyStoreInterpreter ::
  (Member (State (Map UserKey UserId)) r) =>
  InterpreterFor UserKeyStore r
staticUserKeyStoreInterpreter = interpret $ \case
  LookupKey key -> do
    gets (M.lookup key)
  InsertKey uid key ->
    modify $ M.insert key uid
  DeleteKey key ->
    modify $ M.delete key
  DeleteKeyForUser uid key ->
    modify $ M.filterWithKey (\k u -> k /= key && u /= uid)
  ClaimKey key uid -> do
    keys <- get
    let free = M.notMember key keys || M.lookup key keys == (Just uid)
    when free $
      modify $
        M.insert key uid
    pure free
  KeyAvailable key uid -> do
    keys <- get
    pure $ M.notMember key keys || M.lookup key keys == uid
