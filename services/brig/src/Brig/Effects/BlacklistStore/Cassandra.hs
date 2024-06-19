module Brig.Effects.BlacklistStore.Cassandra
  ( interpretBlacklistStoreToCassandra,
  )
where

import Brig.Data.UserKey
import Brig.Effects.BlacklistStore (BlacklistStore (..))
import Cassandra
import Imports
import Polysemy

interpretBlacklistStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (BlacklistStore ': r) a ->
  Sem r a
interpretBlacklistStoreToCassandra =
  interpret $
    embed @m . \case
      Insert uk -> insert uk
      Exists uk -> exists uk
      Delete uk -> delete uk

--------------------------------------------------------------------------------
-- UserKey blacklisting

insert :: (MonadClient m) => UserKey -> m ()
insert uk = retry x5 $ write keyInsert (params LocalQuorum (Identity $ keyText uk))

exists :: (MonadClient m) => UserKey -> m Bool
exists uk =
  (pure . isJust) . fmap runIdentity
    =<< retry x1 (query1 keySelect (params LocalQuorum (Identity $ keyText uk)))

delete :: (MonadClient m) => UserKey -> m ()
delete uk = retry x5 $ write keyDelete (params LocalQuorum (Identity $ keyText uk))

keyInsert :: PrepQuery W (Identity Text) ()
keyInsert = "INSERT INTO blacklist (key) VALUES (?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM blacklist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM blacklist WHERE key = ?"
