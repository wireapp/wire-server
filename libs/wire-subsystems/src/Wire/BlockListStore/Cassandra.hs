module Wire.BlockListStore.Cassandra
  ( interpretBlockListStoreToCassandra,
  )
where

import Cassandra
import Imports
import Polysemy
import Wire.API.AWS.Types
import Wire.BlockListStore (BlockListStore (..))
import Wire.UserKeyStore

interpretBlockListStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (BlockListStore ': r) a ->
  Sem r a
interpretBlockListStoreToCassandra =
  interpret $
    embed @m . \case
      Insert uk event -> insert uk event
      Exists uk -> exists uk
      Delete uk -> delete uk

--------------------------------------------------------------------------------
-- UserKey block listing

insert :: (MonadClient m) => EmailKey -> Maybe SESOriginalEvent -> m ()
insert uk event = retry x5 $ write keyInsert (params LocalQuorum (emailKeyUniq uk, event))

exists :: (MonadClient m) => EmailKey -> m Bool
exists uk =
  (pure . isJust) . fmap runIdentity
    =<< retry x1 (query1 keySelect (params LocalQuorum (Identity $ emailKeyUniq uk)))

delete :: (MonadClient m) => EmailKey -> m ()
delete uk = retry x5 $ write keyDelete (params LocalQuorum (Identity $ emailKeyUniq uk))

keyInsert :: PrepQuery W (Text, Maybe SESOriginalEvent) ()
keyInsert = "INSERT INTO blacklist (key, event) VALUES (?, ?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM blacklist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM blacklist WHERE key = ?"
