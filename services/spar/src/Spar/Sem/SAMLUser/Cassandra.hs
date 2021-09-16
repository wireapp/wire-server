{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.SAMLUser.Cassandra where

import Spar.Sem.SAMLUser
import Polysemy
import Cassandra
import Imports
import qualified Spar.Data as Data


samlUserToCassandra
    :: forall m r a
     . (MonadClient m, Member (Embed m) r) => Sem (SAMLUser ': r) a
    -> Sem r a
samlUserToCassandra = interpret $ embed . \case
  Insert ur uid      -> Data.insertSAMLUser ur uid
  Get ur             -> Data.getSAMLUser ur
  GetAnyByIssuer is  -> Data.getSAMLAnyUserByIssuer is
  GetSomeByIssuer is -> Data.getSAMLSomeUsersByIssuer is
  DeleteByIssuer is  -> Data.deleteSAMLUsersByIssuer is
  Delete uid ur      -> Data.deleteSAMLUser uid ur


interpretClientToIO :: Member (Final IO) r => ClientState -> Sem (Embed Client ': r) a -> Sem r a
interpretClientToIO ctx = interpret $ \case
  Embed action -> embedFinal $ runClient ctx action

