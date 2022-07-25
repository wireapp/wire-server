{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.BlacklistStore.Cassandra where

import qualified Brig.Data.Blacklist as Data
import Brig.Effects.BlacklistStore
import Cassandra (MonadClient)
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
      Insert uk -> Data.insert uk
      Exists uk -> Data.exists uk
      Delete uk -> Data.delete uk
