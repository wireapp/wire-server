{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.SAMLUserStore.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.SAMLUserStore

samlUserStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (SAMLUserStore ': r) a ->
  Sem r a
samlUserStoreToCassandra =
  interpret $
    embed . \case
      Insert ur uid -> Data.insertSAMLUser ur uid
      Get ur -> Data.getSAMLUser ur
      GetAnyByIssuer is -> Data.getSAMLAnyUserByIssuer is
      GetSomeByIssuer is -> Data.getSAMLSomeUsersByIssuer is
      DeleteByIssuer is -> Data.deleteSAMLUsersByIssuer is
      Delete uid ur -> Data.deleteSAMLUser uid ur

interpretClientToIO :: Member (Final IO) r => ClientState -> Sem (Embed Client ': r) a -> Sem r a
interpretClientToIO ctx = interpret $ \case
  Embed action -> embedFinal $ runClient ctx action
