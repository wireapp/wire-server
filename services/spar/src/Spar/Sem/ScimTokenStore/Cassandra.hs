{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.ScimTokenStore.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.ScimTokenStore

scimTokenStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ScimTokenStore ': r) a ->
  Sem r a
scimTokenStoreToCassandra =
  interpret $ embed @m . \case
    Insert st sti -> Data.insertScimToken st sti
    Lookup st -> Data.lookupScimToken st
    GetByTeam tid -> Data.getScimTokens tid
    Delete tid ur -> Data.deleteScimToken  tid ur

