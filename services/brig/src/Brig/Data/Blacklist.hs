{-# LANGUAGE OverloadedStrings #-}

module Brig.Data.Blacklist where

import Imports
import Brig.Data.UserKey
import Brig.Types.Common
import Cassandra

--------------------------------------------------------------------------------
-- UserKey blacklisting

insert :: MonadClient m => UserKey -> m ()
insert uk = retry x5 $ write keyInsert (params Quorum (Identity $ keyText uk))

exists :: MonadClient m => UserKey -> m Bool
exists uk = return . isJust =<< fmap runIdentity <$>
    retry x1 (query1 keySelect (params Quorum (Identity $ keyText uk)))

delete :: MonadClient m => UserKey -> m ()
delete uk = retry x5 $ write keyDelete (params Quorum (Identity $ keyText uk))

keyInsert :: PrepQuery W (Identity Text) ()
keyInsert = "INSERT INTO blacklist (key) VALUES (?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM blacklist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM blacklist WHERE key = ?"

--------------------------------------------------------------------------------
-- Excluded phone prefixes

insertPrefix :: MonadClient m => PhonePrefix -> m ()
insertPrefix p = retry x5 $ write prefixInsert (params Quorum (Identity $ fromPhonePrefix p))

deletePrefix :: MonadClient m => PhonePrefix -> m ()
deletePrefix p = retry x5 $ write prefixDelete (params Quorum (Identity $ fromPhonePrefix p))

existsPrefix :: MonadClient m => PhonePrefix -> m Bool
existsPrefix uk = return . isJust =<< fmap runIdentity <$>
    retry x1 (query1 prefixSelect (params Quorum (Identity $ fromPhonePrefix uk)))

prefixInsert :: PrepQuery W (Identity Text) ()
prefixInsert = "INSERT INTO excluded_phones (prefix) VALUES (?)"

prefixSelect :: PrepQuery R (Identity Text) (Identity Text)
prefixSelect = "SELECT prefix FROM excluded_phones WHERE prefix = ?"

prefixDelete :: PrepQuery W (Identity Text) ()
prefixDelete = "DELETE FROM excluded_phones WHERE prefix = ?"
