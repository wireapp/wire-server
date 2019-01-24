{-# LANGUAGE OverloadedStrings #-}

module Brig.Data.Blacklist
    ( -- * UserKey blacklisting
      insert
    , exists
    , delete

      -- * PhonePrefix excluding
    , insertPrefix
    , deletePrefix
    , existsAnyPrefix
    , getAllPrefixes
    ) where

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
insertPrefix prefix = retry x5 $ write ins (params Quorum (Identity $ fromPhonePrefix prefix))
  where
    ins :: PrepQuery W (Identity Text) ()
    ins = "INSERT INTO excluded_phones (prefix) VALUES (?)"

deletePrefix :: MonadClient m => PhonePrefix -> m ()
deletePrefix prefix = retry x5 $ write del (params Quorum (Identity $ fromPhonePrefix prefix))
  where
    del :: PrepQuery W (Identity Text) ()
    del = "DELETE FROM excluded_phones WHERE prefix = ?"

getAllPrefixes :: MonadClient m => PhonePrefix -> m [PhonePrefix]
getAllPrefixes prefix = do
    let prefixes = fromPhonePrefix <$> allPrefixes (fromPhonePrefix prefix)
    selectPrefixes prefixes

existsAnyPrefix :: MonadClient m => Phone -> m Bool
existsAnyPrefix phone = do
    let prefixes = fromPhonePrefix <$> allPrefixes (fromPhone phone)
    (not . null) <$> selectPrefixes prefixes

selectPrefixes :: MonadClient m => [Text] -> m [PhonePrefix]
selectPrefixes prefixes = do
    results <- fmap runIdentity <$> retry x1 (query sel (params Quorum (Identity $ prefixes)))
    return (PhonePrefix <$> results)
  where
    sel :: PrepQuery R (Identity [Text]) (Identity Text)
    sel = "SELECT prefix FROM excluded_phones WHERE prefix IN ?"
