module Brig.Effects.BlacklistPhonePrefixStore.Cassandra
  ( interpretBlacklistPhonePrefixStoreToCassandra,
  )
where

import Brig.Effects.BlacklistPhonePrefixStore
import Brig.Types.Common
import Cassandra
import Imports
import Polysemy
import Wire.API.User.Identity

interpretBlacklistPhonePrefixStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (BlacklistPhonePrefixStore ': r) a ->
  Sem r a
interpretBlacklistPhonePrefixStoreToCassandra =
  interpret $
    embed @m . \case
      Delete pp -> deletePrefix pp
      ExistsAny uk -> existsAnyPrefix uk
      GetAll pp -> getAllPrefixes pp

--------------------------------------------------------------------------------
-- Excluded phone prefixes

deletePrefix :: (MonadClient m) => PhonePrefix -> m ()
deletePrefix prefix = retry x5 $ write del (params LocalQuorum (Identity prefix))
  where
    del :: PrepQuery W (Identity PhonePrefix) ()
    del = "DELETE FROM excluded_phones WHERE prefix = ?"

getAllPrefixes :: (MonadClient m) => PhonePrefix -> m [ExcludedPrefix]
getAllPrefixes prefix = do
  let prefixes = fromPhonePrefix <$> allPrefixes (fromPhonePrefix prefix)
  selectPrefixes prefixes

existsAnyPrefix :: (MonadClient m) => Phone -> m Bool
existsAnyPrefix phone = do
  let prefixes = fromPhonePrefix <$> allPrefixes (fromPhone phone)
  not . null <$> selectPrefixes prefixes

selectPrefixes :: (MonadClient m) => [Text] -> m [ExcludedPrefix]
selectPrefixes prefixes = do
  results <- retry x1 (query sel (params LocalQuorum (Identity $ prefixes)))
  pure $ uncurry ExcludedPrefix <$> results
  where
    sel :: PrepQuery R (Identity [Text]) (PhonePrefix, Text)
    sel = "SELECT prefix, comment FROM excluded_phones WHERE prefix IN ?"
