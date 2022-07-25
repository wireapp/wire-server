{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistPhonePrefixStore.Cassandra where

import qualified Brig.Data.Blacklist as Data
import Brig.Effects.BlacklistPhonePrefixStore
import Cassandra (MonadClient)
import Imports
import Polysemy

interpretBlacklistPhonePrefixStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (BlacklistPhonePrefixStore ': r) a ->
  Sem r a
interpretBlacklistPhonePrefixStoreToCassandra =
  interpret $
    embed @m . \case
      Insert ep -> Data.insertPrefix ep
      Delete pp -> Data.deletePrefix pp
      ExistsAny uk -> Data.existsAnyPrefix uk
      GetAll pp -> Data.getAllPrefixes pp
