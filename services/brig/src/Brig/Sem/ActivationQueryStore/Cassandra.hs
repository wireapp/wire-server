-- |
module Brig.Sem.ActivationQueryStore.Cassandra
  ( activationQueryStoreToCassandra,
  )
where

import Brig.Data.Instances ()
import Brig.Sem.ActivationQueryStore
import Brig.Types
import Cassandra
import Data.Id
import Imports
import Polysemy

activationQueryStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ActivationQueryStore ': r) a ->
  Sem r a
activationQueryStoreToCassandra = interpret $ \case
  (KeyInsert ak txt txt' ac m_id n i) ->
    embed @m . retry x5 . write keyInsertQuery . params LocalQuorum $ (ak, txt, txt', ac, m_id, n, i)
  (KeySelect key) -> embed @m . retry x1 . query1 keySelectQuery $ params LocalQuorum (Identity key)
  (CodeSelect ak) -> embed @m . retry x1 . query1 codeSelectQuery . params LocalQuorum . Identity $ ak
  (KeyDelete ak) -> embed @m . write keyDeleteQuery . params LocalQuorum . Identity $ ak

keyInsertQuery :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
keyInsertQuery =
  "INSERT INTO activation_keys \
  \(key, key_type, key_text, code, user, retries) VALUES \
  \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

keySelectQuery :: PrepQuery R (Identity ActivationKey) (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)
keySelectQuery = "SELECT ttl(code) as ttl, key_type, key_text, code, user, retries FROM activation_keys WHERE key = ?"

codeSelectQuery :: PrepQuery R (Identity ActivationKey) (Maybe UserId, ActivationCode)
codeSelectQuery = "SELECT user, code FROM activation_keys WHERE key = ?"

keyDeleteQuery :: PrepQuery W (Identity ActivationKey) ()
keyDeleteQuery = "DELETE FROM activation_keys WHERE key = ?"
