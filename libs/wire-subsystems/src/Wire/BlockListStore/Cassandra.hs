module Wire.BlockListStore.Cassandra
  ( interpretBlockListStoreToCassandra,
  )
where

import Cassandra
import Imports
import Polysemy
import Wire.BlockListStore (BlockListStore (..))
import Wire.UserKeyStore

interpretBlockListStoreToCassandra ::
  forall r.
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor BlockListStore r
interpretBlockListStoreToCassandra casClient =
  interpret $
    embed @IO . runClient casClient . \case
      Insert uk -> insert uk
      Exists uk -> exists uk
      Delete uk -> delete uk

--------------------------------------------------------------------------------
-- UserKey block listing

insert :: (MonadClient m) => EmailKey -> m ()
insert uk = retry x5 $ write keyInsert (params LocalQuorum (Identity $ emailKeyUniq uk))

exists :: (MonadClient m) => EmailKey -> m Bool
exists uk =
  (pure . isJust) . fmap runIdentity
    =<< retry x1 (query1 keySelect (params LocalQuorum (Identity $ emailKeyUniq uk)))

delete :: (MonadClient m) => EmailKey -> m ()
delete uk = retry x5 $ write keyDelete (params LocalQuorum (Identity $ emailKeyUniq uk))

keyInsert :: PrepQuery W (Identity Text) ()
keyInsert = "INSERT INTO blacklist (key) VALUES (?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM blacklist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM blacklist WHERE key = ?"
