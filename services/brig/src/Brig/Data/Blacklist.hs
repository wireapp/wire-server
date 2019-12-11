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
exists uk = isJust <$> retry x1 (query1 keySelect (params Quorum (Identity $ keyText uk)))

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

insertPrefix :: MonadClient m => ExcludedPrefix -> m ()
insertPrefix prefix = retry x5 $ write ins (params Quorum (phonePrefix prefix, comment prefix))
  where
    ins :: PrepQuery W (PhonePrefix, Text) ()
    ins = "INSERT INTO excluded_phones (prefix, comment) VALUES (?, ?)"

deletePrefix :: MonadClient m => PhonePrefix -> m ()
deletePrefix prefix = retry x5 $ write del (params Quorum (Identity prefix))
  where
    del :: PrepQuery W (Identity PhonePrefix) ()
    del = "DELETE FROM excluded_phones WHERE prefix = ?"

getAllPrefixes :: MonadClient m => PhonePrefix -> m [ExcludedPrefix]
getAllPrefixes prefix = do
    let prefixes = fromPhonePrefix <$> allPrefixes (fromPhonePrefix prefix)
    selectPrefixes prefixes

existsAnyPrefix :: MonadClient m => Phone -> m Bool
existsAnyPrefix phone = do
    let prefixes = fromPhonePrefix <$> allPrefixes (fromPhone phone)
    (not . null) <$> selectPrefixes prefixes

selectPrefixes :: MonadClient m => [Text] -> m [ExcludedPrefix]
selectPrefixes prefixes = do
    results <- retry x1 (query sel (params Quorum (Identity $ prefixes)))
    return $ (\(p, c) -> ExcludedPrefix p c) <$> results
  where
    sel :: PrepQuery R (Identity [Text]) (PhonePrefix, Text)
    sel = "SELECT prefix, comment FROM excluded_phones WHERE prefix IN ?"
