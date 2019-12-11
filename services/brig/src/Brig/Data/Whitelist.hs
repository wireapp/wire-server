module Brig.Data.Whitelist
    ( insert
    , exists
    , delete
    ) where

import Imports
import Brig.Data.UserKey
import Cassandra

insert :: MonadClient m => UserKey -> m ()
insert uk = retry x5 $ write keyInsert $ params Quorum $ Identity $ keyText uk

exists :: MonadClient m => UserKey -> m Bool
exists uk = isJust <$> (retry x1 $ query1 keySelect $ params Quorum $ Identity $ keyText uk)

delete :: MonadClient m => UserKey -> m ()
delete uk = retry x5 $ write keyDelete $ params Quorum $ Identity $ keyText uk

keyInsert :: PrepQuery W (Identity Text) ()
keyInsert = "INSERT INTO whitelist (key) VALUES (?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM whitelist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM whitelist WHERE key = ?"
