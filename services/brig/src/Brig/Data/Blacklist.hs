{-# LANGUAGE OverloadedStrings #-}

module Brig.Data.Blacklist where

import Brig.Data.UserKey
import Cassandra
import Data.Functor.Identity
import Data.Maybe (isJust)
import Data.Text (Text)

insert :: MonadClient m => UserKey -> m ()
insert uk = retry x5 $ write keyInsert (params Quorum (Identity $ keyText uk))

exists :: MonadClient m => UserKey -> m Bool
exists uk = return . isJust =<< fmap runIdentity <$>
    retry x1 (query1 keySelect (params Quorum (Identity $ keyText uk)))

delete :: MonadClient m => UserKey -> m ()
delete uk = retry x5 $ write keyDelete (params Quorum (Identity $ keyText uk))

--------------------------------------------------------------------------------
-- Queries

keyInsert :: PrepQuery W (Identity Text) ()
keyInsert = "INSERT INTO blacklist (key) VALUES (?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM blacklist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM blacklist WHERE key = ?"
